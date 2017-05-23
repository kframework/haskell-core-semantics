require "haskell-core-syntax.k"

module HASKELL-CORE-EXECUTION
imports HASKELL-CORE-SYNTAX
imports INT
imports K-REFLECTION
imports MAP

syntax Locs ::= List{Loc,";"}
syntax Loc ::= Int

syntax Object ::= data(DataCon,Locs)
              | abs(CoreBndr,Expr,Map)
              | thunk(Expr,Map)

syntax KResult ::= val(Loc)
syntax Expr ::= KResult
syntax K ::= doBind(Id,Expr,K)
           | bindVals(Ids,Locs,K)
           | restoreEnv(Map) // environment to reinstate
syntax Ids ::= List{Id,";"}
syntax Ids ::= getTmVars(VarList) [function]
rule getTmVars(VarEmpty) => .Ids
rule getTmVars(VarCons(tmVar(_,coreBndr(I)),Vars)) => I;getTmVars(Vars)
rule getTmVars(VarCons(tyVar(_,_),Vars)) => getTmVars(Vars)

configuration
  <k>$PGM:K</k>
  <store>.Map</store>
  <env>.Map</env>
  <frames>.List</frames>
  <exit-code exit="">0::Int</exit-code>

rule
  app(lam(coreBndr(V),E1),E2) => doBind(V,E2,E1)
rule
  cast(E,_) => E
rule
  case(val(V),tmVar(_,coreBndr(R)),_,altCons(alt(defaultAlt(),_,E),_)) =>
    doBind(R,val(V),E)
rule
  <k>case(val(V),tmVar(_,coreBndr(R)),_,altCons(alt(dataAlt(C),Vars,E),_)) =>
    doBind(R,val(V),bindVals(getTmVars(Vars),Fields,E))...</k>
  <store>... V |-> data(C,Fields) ...</store>
rule
  <k>case(val(V),R,_,(altCons(alt(dataAlt(C1),_,E),Alts)=>Alts))...</k>
  <store>... V |-> data(C2,_) ...</store>
  when C1 =/=K C2

rule
  case(lit(L),tmVar(_,coreBndr(R)),_,altCons(alt(litAlt(L),_,E),_)) => doBind(R,lit(L),E)
rule
  case(lit(L1),_,_,(altCons(alt(litAlt(L2),_,E),Alts) => Alts))
  when L1 =/=K L2

syntax LocDef ::= locDef(Int,Expr)
syntax LocDefs ::= List{LocDef,";"}
syntax K ::= bindRec(BindingList,LocDefs,Expr)

rule
  <k>let(nonRec(tmVar(_,V),E2),E1) => E1 ...</k>
  <store>... .Map => !L:Int |-> thunk(E2,Env) ...</store>
  <env>Env => Env [ V <- !L ] </env>
rule
  <k>doBind(V,val(L),Body) => Body ~> restoreEnv(Env)...</k>
  <env>Env (. => V |-> val(L)) </env>

rule
  let(rec(Binds),E) => bindRec(Binds,.LocDefs,E)
rule
  bindRec((bind(tyVar(_,_),_,Binds) => Binds),_,_)
rule
  <k>bindRec((bind(tmVar(_,V),Def,Binds)=> Binds),(L => locDef(!Loc,Def);L), E)...</k>
  <env>Env => Env [ V <- !Loc ]</env>
rule
  <k>bindRec(emptyBind,LocDefs,E) => E...</k>
  <env>Env</env>
  <store>... (. => mkRecStore(Env,LocDefs)) ...</store>

syntax Map ::= mkRecStore(Map,LocDefs) [function]
rule mkRecStore(Env,.LocDefs) => .Map
rule mkRecStore(Env, locDef(L,D) ; LocDefs) => (L |-> thunk(D,Env)) mkRecStore(Env,LocDefs)

endmodule
