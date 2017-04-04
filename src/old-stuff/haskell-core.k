require "haskell-core-syntax.k"

module HASKELL-CORE
    imports HASKELL-CORE-SYNTAX

    syntax KResult ::= Type

    configuration <T>
                    <k> $PGM:Expr </k>
                    <tenv> .Map </tenv>
                  </T>

    rule <k> X:Id => T ...</k>
         <tenv>... X |-> T ...</tenv>

    syntax Expr ::= Int

    syntax Expr ::= Type
    syntax Type ::= "typeVar"
                  | "typeApp"
                  | "typeConApp"
                  | "forAllTypeAnon"
                  | "forAllTypeNamed"
                  | "kindCast"
                  | "typeCoercion"

    rule _:TypeVariable => typeVar
    rule _:TypeApplication => typeApp
    rule _:TypeConApplication => typeConApp
    rule _:ForAllTypeAnonymous => forAllTypeAnon
    rule _:ForAllTypeNamed => forAllTypeNamed
    rule _:KindCast => kindCast
    rule _:TypeCoercion => typeCoercion

    rule (Tick T:Type) => T // TM_TICK

    rule <k> X:Id => T ...</k>
         <tenv>... X |-> T ...</tenv>

    rule (Lam T1:Type T2:Type) => ForAllTyAnon T1 T2

    rule (App T1:Type T2:Type) => App T1 T2



endmodule
