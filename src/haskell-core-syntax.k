module HASKELL-CORE-SYNTAX

  syntax Var         ::= "tmVar" "(" Type "," Id ")"
                       | "tyVar" "(" Type "," Id ")"

  syntax Lit         ::= "machChar"
                       | "machStr"    "(" String ")"
                       | "nullAddr"
                       | "machInt"    "(" Int ")"
                       | "machInt64"  "(" Int ")"
                       | "machWord"   "(" Int ")"
                       | "machWord64" "(" Int ")"
                       | "machFloat"
                       | "machDouble"

  syntax App         ::= "app" "(" Expr "," Expr ")"

  syntax CoreBndr    ::= "coreBndr" "(" Id ")"

  syntax Lam         ::= "lam" "(" CoreBndr "," Expr ")"

  syntax BindingList ::= "emptyBind"
                       | "bind" "(" Var "," Expr "," BindingList ")"

  syntax Binding  ::= "nonRec" "(" Var "," Expr ")"
                    | "rec" "(" BindingList ")"

  syntax Let      ::= "let" "(" Binding "," Expr ")"

  syntax VarList  ::= "VarEmpty"
                    | "VarCons" "(" VarList ")"

  syntax Name     ::= String

  syntax DataCon  ::= "dataCon" "(" Name ")"

  syntax AltCon   ::= "dataAlt" "(" DataCon ")"
                    | "litAlt" "(" Lit ")"
                    | "defaultAlt" "(" ")"

  syntax Alt      ::= "alt" "(" AltCon "," VarList "," Expr ")"

  syntax Case     ::= "case" "(" Expr "," Var "," Type "," Alt ")"

  syntax Cast     ::= "cast" "(" Expr "," Coercion ")"

  syntax Tickish  ::= "profNote"    "(" ")"
                    | "hpcTick"     "(" ")"
                    | "breakpoint"  "(" ")"
                    | "sourceNote"  "(" ")"

  syntax Tick     ::= "tick" "(" Tickish "," Expr ")"

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
