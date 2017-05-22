module HASKELL-CORE-SYNTAX

  syntax Type ::= "<type>"

  syntax Var  ::= "tmVar" "(" Type "," Id ")"
                | "tyVar" "(" Type "," Id ")"

  syntax Lit  ::= "machChar"
                | "machStr"    "(" String ")"
                | "nullAddr"
                | "machInt"    "(" Int ")"
                | "machInt64"  "(" Int ")"
                | "machWord"   "(" Int ")"
                | "machWord64" "(" Int ")"
                | "machFloat"
                | "machDouble"

  syntax Expr ::= Var
                | Lit
                | App
                | Lam
                | Let
                | Case
                | Cast
                | Tick
                | Type
                | Coercion
endmodule
