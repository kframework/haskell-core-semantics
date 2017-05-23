module HASKELL-CORE-SYNTAX

  syntax Var         ::= "tmVar" "(" Type "," Id ")"
                       | "tyVar" "(" Type "," Id ")"

  syntax Lit         ::= "machChar"         /* TODO: Native support for chars */
                       | "machStr"    "(" String ")"
                       | "nullAddr"
                       | "machInt"    "(" Int ")"
                       | "machInt64"  "(" Int ")"
                       | "machWord"   "(" Int ")"
                       | "machWord64" "(" Int ")"
                       | "machFloat"   /* TODO: Native support for rationals. */
                       | "machDouble"  /* TODO: Native support for rationals. */

  syntax App         ::= "app" "(" Expr "," Expr ")"

  syntax CoreBndr    ::= "coreBndr" "(" Id ")"

  syntax Lam         ::= "lam" "(" CoreBndr "," Expr ")"

  syntax BindingList ::= "emptyBind"
                       | "bind" "(" Var "," Expr "," BindingList ")"

  syntax Binding  ::= "nonRec" "(" Var "," Expr ")"
                    | "rec" "(" BindingList ")"

  syntax Let      ::= "let" "(" Binding "," Expr ")"

  syntax VarList  ::= "VarEmpty"
                    | "VarCons" "(" Var "," VarList ")"

  syntax Name     ::= String

  syntax DataCon  ::= "dataCon" "(" Name ")"

  syntax AltCon   ::= "dataAlt" "(" DataCon ")"
                    | "litAlt" "(" Lit ")"
                    | "defaultAlt" "(" ")"

  syntax Alt      ::= "alt" "(" AltCon "," VarList "," Expr ")"
  syntax Alts     ::= List{Alt,";"}

  syntax Case     ::= "case" "(" Expr "," Var "," Type "," Alt ")"

  syntax Cast     ::= "cast" "(" Expr "," Coercion ")"

  syntax Tickish  ::= "profNote"    "(" ")"
                    | "hpcTick"     "(" ")"
                    | "breakpoint"  "(" ")"
                    | "sourceNote"  "(" ")"

  syntax Tick     ::= "tick" "(" Tickish "," Expr ")"

  /* TODO: type system will eventually be implemented. */
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
