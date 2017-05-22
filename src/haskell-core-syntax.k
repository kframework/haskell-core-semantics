module HASKELL-CORE-SYNTAX

  syntax Var  ::= "tmVar" "(" Type "," Id ")"
                | "tyVar" "(" Type "," Id ")"

  syntax Lit      ::= "machChar"
                    | "machStr"    "(" String ")"
                    | "nullAddr"
                    | "machInt"    "(" Int ")"
                    | "machInt64"  "(" Int ")"
                    | "machWord"   "(" Int ")"
                    | "machWord64" "(" Int ")"
                    | "machFloat"
                    | "machDouble"

  syntax App      ::= "app" "(" Expr "," Expr ")"

  syntax CoreBndr ::= "coreBndr" "(" Id ")"

  syntax Lam      ::= "lam" "(" CoreBndr "," Expr ")"

  syntax BindingList ::= "emptyBind"
                       | "bind" "(" Var "," Expr "," BindingList ")"

  syntax Binding  ::= "nonRec" "(" Var "," Expr ")"
                    | "rec" "(" BindingList ")"

  syntax Let      ::= "let" "(" Binding "," Expr ")"

  syntax Alt      ::= "TODO"

  syntax Case     ::= "case" "(" Expr "," Var "," Type "," Alt ")"

  syntax Cast     ::= "TODO"

  syntax Tick     ::= "TODO"

  syntax Type     ::= "<type>"

  syntax Coercion ::= "TODO"

  syntax Expr     ::= Var
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
