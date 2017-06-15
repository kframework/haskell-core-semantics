require "haskell-core-syntax.k"

module HASKELL-CORE-EXECUTION
  imports HASKELL-CORE-COMMON
  imports INT
  imports K-REFLECTION
  imports MAP

  // Addresses in the store
  syntax Locs       ::= List{Loc, ";"} [klabel(locList)]
  syntax Loc        ::= Int
                      | freshLoc(Int)  [freshGenerator, function]

  rule freshLoc(X:Int) => X

  // Contents of the store
  syntax Object     ::= data(DataCon, Locs)
                      | closure(Name, Expr, Map) // closure
                      | tyabs(Expr, Map) // type abstraction closure
                      | thunk(Expr, Map)
                      | indirection(Loc)
                      | litObj(LitExpr)

  syntax KResult    ::= val(Loc) /* val should be used only
                                 with evaluated constants. */
                      | LitExpr

  syntax Expr       ::= KResult

  // doBind: binds Id to Expr (for evaluation in K) without forcing
  // the evaluation of Expr.
  syntax K          ::= doBind(Name, Expr, K)
                      | doBindMap(Name, Expr, Map, K)
                      | bindVals(Names, Locs, K)
                      | scope(K)
                      | restoreEnv(Map) // environment to reinstate
                      | force(Expr, Map, Loc)
                      | update(Expr, Loc) [strict(1)]

  syntax Names      ::= List{Name, ";"} [klabel(nameList)]
  syntax Names      ::= getTmVars(VarList) [function]

  syntax K ::= evalDecl(SeqDecl, Expr)                               [function]

  rule evalDecl(nonRec(tmVar(_, R), E1), E2:Expr)
       => doBind(R, E1, E2)
  rule evalDecl(nonRec(tmVar(_, R), E1) DS:SeqDecl, E2:Expr)
       => doBind(R, E1, evalDecl(DS, E2))
  rule evalDecl(algTyCon(_, _, dataTyCon(DataConEmpty)), E:Expr)
       => E
  rule evalDecl(algTyCon(_, _, dataTyCon(DataConEmpty)) DS:SeqDecl, E:Expr)
       => evalDecl(DS, E)
  rule
    <k>
      evalDecl(algTyCon(TN, TAU, dataTyCon(DataConCons(dataCon(R, Ar), DCS))),
               E:Expr)
      => evalDecl(algTyCon(TN, TAU, dataTyCon(DCS)), E)
      ...
    </k>
    <tenv>
      M:Map => M[R <- Ar]
    </tenv>
  rule
    <k>
      evalDecl(algTyCon(TN, TAU, dataTyCon(DataConCons(dataCon(R, Ar), DCS)))
               DS:SeqDecl,
               E:Expr)
      => evalDecl(algTyCon(TN, TAU, dataTyCon(DCS)) DS, E)
      ...
    </k>
    <tenv>
      M:Map => M[R <- Ar]
    </tenv>

  rule DS:SeqDecl E:Expr => evalDecl(DS, E)

  // Get Ids out of VarList
  rule getTmVars(VarEmpty)                   => .Names
  rule getTmVars(VarCons(tmVar(_, N), Vars)) => N; getTmVars(Vars)
  rule getTmVars(VarCons(tyVar(_, _), Vars)) => getTmVars(Vars)

  // NB: Some rules in core_spec.pdf are accounted for due to strictness of the
  // literal case, i.e. S_Case, S_Tick, S_App, etc.

  configuration
    <T>
      <k> $PGM:K </k>
      <store> .Map </store> // Loc  |-> Object
      <env> .Map </env>     // Name |-> Loc
      <tenv> .Map </tenv>   // Name |-> Arity
      <exit-code exit=""> 0::Int </exit-code>
    </T>

  /*------------------------------------------------------------
  RULES FOR VAR
  ------------------------------------------------------------*/
  rule
    <k> var(tmVar(_, R)) => val(L) ... </k>
    <store> ... L |-> data(_, _) ... </store>
    <env> ... R |-> L ... </env>
  rule
    <k> var(tmVar(_, R)) => val(L) ... </k>
    <store> ... L |-> closure(_, _, _) ... </store>
    <env> ... R |-> L ... </env>
  rule
    <k> var(tmVar(_, R)) => val(L) ... </k>
    <store> ... L |-> tyabs(_, _) ... </store>
    <env> ... R |-> L ... </env>
  rule
    <k> var(tmVar(_, R)) => val(L) ... </k>
    <store> ... L |-> litObj(_) ... </store>
    <env> ... R |-> L ... </env>
  rule // could shortcut the store entry also
    <k> var(tmVar(_, R)) ... </k>
    <store> ... L |-> indirection(L') ... </store>
    <env> ... R |-> (L => L') ... </env>
  rule
    <k> (. => force(E, Env, L)) ~> var(tmVar(_, R)) ... </k>
    <store> ... L |-> thunk(E, Env) ... </store>
    <env> ... R |-> L ... </env>
  rule
     <k> force(E, Env, L) => update(E, L) ~> restoreEnv(EOld) ... </k>
     <env> EOld => Env </env>
  rule
    <k> update(val(L'), L) => . ... </k>
    <store> ... L |-> (_ => indirection(L')) ... </store>
  rule
    <k> update(lit(M), L) => . ... </k>
    <store> ... L |-> (_ => litObj(lit(M))) ... </store>

  // App
  rule
    <k>
      app(val(L), E2) => doBindMap(R, E2, Env, Body) ~> restoreEnv(EOld) ...
    </k>
    <env>
      EOld
    </env>
    <store>
      ... L |-> closure(R, Body, Env) ...
    </store>
  rule
    <k> app(val(L), _) => Body ~> restoreEnv(EOld) ... </k>
    <env> EOld => Env </env>
    <store> ... L |-> tyabs(Body, Env) ... </store>

  // Lam
  rule
    <k> lam(tyVar(_, _), Body) => val(!L) ... </k>
    <env> Env </env>
    <store> ... (. => !L |-> tyabs(Body, Env)) ... </store>
  rule
    <k> lam(tmVar(_, R), Body) => val(!L) ... </k>
    <env> Env </env>
    <store> ... (. => !L |-> closure(R, Body, Env)) ... </store>

  // Let
  /* TODO: Let's might need flattening and so on (look at S_LetRecFlat). */
  rule let(nonRec(tmVar(_, V), E2), E1) => scope(doBind(V, E2, E1))
  rule let(rec(Binds), E) => scope(bindRec(Binds, .LocDefs, E))

  // letrec helpers
  syntax LocDef  ::= locDef(Int, Expr)
  syntax LocDefs ::= List{LocDef, ";"} [klabel(locDefList)]
  // `bindRec` seems to be just a list of `doBind`s.
  syntax K       ::= bindRec(BindingList, LocDefs, Expr)

  // Make a new scope with all bindings
  rule bindRec((bind(tyVar(_, _), _, Binds) => Binds), _, _)
  rule
    <k>
      bindRec((bind(tmVar(_, V), Def, Binds) => Binds),
              (L => locDef(!Loc, Def); L), E)
      ...
    </k>
    <env> Env => Env [ V <- !Loc ] </env>
  // once all bindings are processed we have the final environment,
  // create the recursive thunks
  rule
    <k> bindRec(emptyBind, LocDefs, E) => E ... </k>
    <env> Env </env>
    <store> ... (. => mkRecStore(Env, LocDefs)) ... </store>

  // We create a mapping of locations to thunks for lazy evaluation.
  syntax Map ::= mkRecStore(Map, LocDefs) [function]
  rule mkRecStore(Env, .LocDefs) => .Map
  rule mkRecStore(Env, locDef(L, D); LocDefs)
         => (L |-> thunk(D, Env)) mkRecStore(Env, LocDefs)

  // Case
  // default
  rule case(val(L), tmVar(_, R), _, altCons(alt(defaultAlt(), _, E), _))
         => scope(doBind(R, val(L), E))

  // Matching Lit for the default case.
  rule case(lit(M), tmVar(_, R), _, altCons(alt(defaultAlt(), _, E), _))
         => scope(doBind(R, lit(M), E))

  // matching constructor
  rule
    <k>
      case(val(V), tmVar(_, R), _, altCons(alt(dataAlt(C), Vars, E), _))
        => scope(doBind(R, val(V), bindVals(getTmVars(Vars), Fields, E)))
      ...
    </k>
    <store>
      ...
      V |-> data(C, Fields)
      ...
    </store>

  // different constructor
  rule
    <k>
      case(val(V), R, _, (altCons(alt(dataAlt(C1), _, E), AltList)
             => AltList))
      ...
    </k>
    <store>
      ...
      V |-> data(C2, _)
      ...
    </store>
    requires C1 =/=K C2

  // matching lit
  rule case(lit(L), tmVar(_, R), _, altCons(alt(litAlt(L), _, E), _))
         => scope(doBind(R, lit(L), E))
  // different lit
  rule
    case(lit(L1), _, _, (altCons(alt(litAlt(L2), _, E), AltList) => AltList))
    requires L1 =/=K L2

  // Cast
  rule cast(E, _) => E

  // Tick (debugging info)
  rule tick(_, E) => E

  // Type
  // does not need any evaluation rules,
  // all type applications should discard the argument.

  rule <k> restoreEnv(Env) => . ... </k>
       <env> _ => Env </env>
  rule <k> val(I) ~> restoreEnv(Env) => val(I) ... </k>
       <env> _ => Env </env>

  rule <k> scope(K) => K ~> restoreEnv(Env) ... </k>
       <env> Env </env>

  syntax Bool ::= isExpVar(Expr) [function]

  rule isExpVar(var(_)) => true
  rule isExpVar(_)      => false [owise]

  rule
    <k> doBind(V, E, Body) => doBindMap(V, E, Env, Body) ... </k>
    <env> Env </env>

  // Temporarily modify the environment to handle `doBind`.
  rule
    <k> doBindMap(V, E, ENew, Body) => Body ... </k>
    <env> Env => ENew[V <- !L:Int] </env>
    <store> ... .Map => !L |-> thunk(E, Env) ... </store>
    requires notBool isExpVar(E)
  rule
    <k> doBindMap(V, lit(M), ENew, Body) => Body ... </k>
    <env> Env => ENew[V <- !L:Int] </env>
    <store> ... .Map => !L |-> litObj(lit(M)) ... </store>
  rule
    <k> doBindMap(V, var(tmVar(_, R)), ENew, Body) => Body ... </k>
    <env> Env:Map (R |-> L) => ENew[V <- L] </env>

  rule
    <k> val(L) => O </k>
    <store> Sto:Map (L |-> O) => .Map </store>
    <env> _ => .Map </env>

endmodule
