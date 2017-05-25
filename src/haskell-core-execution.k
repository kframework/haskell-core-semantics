require "haskell-core-syntax.k"

module HASKELL-CORE-EXECUTION
  imports HASKELL-CORE-SYNTAX
  imports INT
  imports K-REFLECTION
  imports MAP

  // Addresses in the store
  syntax Locs     ::= List{Loc, ";"}
  syntax Loc      ::= Int

  // Contents of the store
  syntax Object   ::= data(DataCon, Locs)
                    | abs(Var, Expr, Map) // closure
                    | thunk(Expr, Map)

  // Store addresses are expressions in WHNF
  syntax KResult  ::= val(Loc)
  syntax Expr     ::= KResult

  // doBind: binds Id to Expr (for evaluation in K) without forcing
  // the evaluation of Expr.
  syntax K        ::= doBind(Id, Expr, K)
                    | bindVals(Ids, Locs, K)
                    | restoreEnv(Map) // environment to reinstate

  syntax Ids      ::= List{Id, ";"}
  syntax Ids      ::= getTmVars(VarList) [function]

  // Get Ids out of VarList
  rule getTmVars(VarEmpty)                             => .Ids
  rule getTmVars(VarCons(tmVar(_, name(I)), Vars)) => I; getTmVars(Vars)
  rule getTmVars(VarCons(tyVar(_, _), Vars))           => getTmVars(Vars)

  // NB: Some rules in core_spec.pdf are accounted for due to strictness of the
  // literal case, i.e. S_Case, S_Tick, S_App, etc.

  configuration
    <k> $PGM:K </k>
    <store> .Map </store>
    <env> .Map </env>
    <exit-code exit=""> 0::Int </exit-code>

  // TODO: check if type application
  // S_Beta: not evaluated immediately due to laziness
  rule app(lam(tmVar(_, name(R)), E1), E2) => doBind(R, E2, E1)

  rule cast(E, _) => E

  // S_MatchDefault
  rule case(val(V), tmVar(_, name(R)), _, altCons(alt(defaultAlt(), _, E), _))
         => doBind(R, val(V), E)

  // S_MatchData (first Alt matches)
  rule
    <k>
      case(val(V), tmVar(_, name(R)), _, altCons(alt(dataAlt(C), Vars, E), _))
        => doBind(R, val(V), bindVals(getTmVars(Vars), Fields, E))
      ...
    </k>
    <store>
      ...
      V |-> data(C, Fields)
      ...
    </store>

  // S_MatchData (first Alt doesn't match)
  rule
    <k> case(val(V), R, _, (altCons(alt(dataAlt(C1), _, E), AltList) => AltList)) ... </k>
    <store> ... V |-> data(C2, _) ...</store>
    requires C1 =/=K C2

  // S_MatchLit (first Alt matches)
  rule case(lit(L), tmVar(_, name(R)), _, altCons(alt(litAlt(L), _, E), _))
         => doBind(R, lit(L), E)

  // S_MatchLit (first Alt doesn't match)
  rule
    case(lit(L1), _, _, (altCons(alt(litAlt(L2), _, E), AltList) => AltList))
    when L1 =/=K L2

  syntax LocDef  ::= locDef(Int, Expr)
  syntax LocDefs ::= List{LocDef, ";"}
  syntax K       ::= bindRec(BindingList, LocDefs, Expr)

  // S_LetNonRec
  rule let(nonRec(tmVar(_, name(V)), E2), E1) => doBind(V, E2, E1)

  rule
    <k> doBind(V, val(L), Body) => Body ~> restoreEnv(Env) ... </k>
    <env> Env (. => V |-> val(L)) </env>

  rule let(rec(Binds),E) => bindRec(Binds,.LocDefs,E)
  rule bindRec((bind(tyVar(_, _), _, Binds) => Binds), _, _)

  rule
    <k>
      bindRec((bind(tmVar(_, V), Def, Binds) => Binds), (L => locDef(!Loc, Def); L), E)
      ...
    </k>
    <env>
      Env => Env [ V <- !Loc ]
    </env>

  rule
    <k> bindRec(emptyBind, LocDefs, E) => E ... </k>
    <env> Env </env>
    <store> ... (. => mkRecStore(Env, LocDefs)) ... </store>

  syntax Map ::= mkRecStore(Map, LocDefs) [function]
  rule mkRecStore(Env, .LocDefs) => .Map
  rule mkRecStore(Env, locDef(L, D); LocDefs)
         => (L |-> thunk(D, Env)) mkRecStore(Env, LocDefs)

endmodule
