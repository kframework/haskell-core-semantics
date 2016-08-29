require "types-syntax.k"

module HASKELL-CORE-SYNTAX

    imports TYPES-SYNTAX

    // The core expression sort as defined in `coreSyn/CoreSyn.lhs`.
    syntax Expr ::= "Var"   Id
                  | "App"   Expr Expr
                  | "Lam"   Id      Expr
                  | "Let"   Binding Expr
                  | "Case"  Expr    Id        Type Alt
                  | "Cast"  Expr    Coercion
                  | "Tick"  Expr                            [strict]
                  | "Type"  Type
                  | "Coercion" Coercion
                  // TODO: Internal note
                  // TODO: Type
                  // TODO: Coercion


    // Bindings for let statements.
    // Defined in coreSyn/CoreSyn.hs:Bind
    syntax Binding ::= Id "=" Expr
    syntax Bindings ::= List{Binding,","}

    // Case alternatives
    syntax Alt ::= Con Names

    syntax Names ::= List{Id,","}

    syntax Con ::= Id
                 | "DEFAULT"

    syntax Program ::= Bindings

endmodule