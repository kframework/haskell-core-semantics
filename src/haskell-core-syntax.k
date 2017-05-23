module HASKELL-CORE-SYNTAX

  syntax Var         ::= "tmVar" "(" Type "," CoreBndr ")"
                       | "tyVar" "(" Type "," CoreBndr ")"

  syntax Lit         ::= "machChar"         /* TODO: Native support for chars */
                       | "machStr"    "(" String ")"
                       | "nullAddr"
                       | "machInt"    "(" Int ")"
                       | "machInt64"  "(" Int ")"
                       | "machWord"   "(" Int ")"
                       | "machWord64" "(" Int ")"
                       | "machFloat"   /* TODO: Native support for rationals. */
                       | "machDouble"  /* TODO: Native support for rationals. */

  syntax App         ::= Expr "," Expr

  syntax CoreBndr    ::= "coreBndr" "(" Id ")"

  syntax Lam         ::= CoreBndr "," Expr

  syntax BindingList ::= "emptyBind"
                       | "bind" "(" Var "," Expr "," BindingList ")"

  syntax Binding  ::= "nonRec" "(" Var "," Expr ")"
                    | "rec" "(" BindingList ")"

  syntax Let      ::= Binding "," Expr

  syntax VarList  ::= "VarEmpty"
                    | "VarCons" "(" Var "," VarList ")"

  syntax Name     ::= Id

  syntax DataCon  ::= "dataCon" "(" Name ")"

  syntax AltCon   ::= "dataAlt" "(" DataCon ")"
                    | "litAlt" "(" Lit ")"
                    | "defaultAlt" "(" ")"

  syntax Alt      ::= "alt" "(" AltCon "," VarList "," Expr ")"

  syntax AltList  ::= "altEmpty"
                    | "altCons" "(" Alt "," AltList ")"

  syntax Case     ::= Expr "," Var "," Type "," AltList

  syntax Cast     ::= Expr "," Coercion

  syntax Tickish  ::= "profNote"    "(" ")"
                    | "hpcTick"     "(" ")"
                    | "breakpoint"  "(" ")"
                    | "sourceNote"  "(" ")"

  syntax Tick     ::= Tickish "," Expr

  /* TODO: type system will eventually be implemented. */
  syntax Type     ::= "<type>"

  syntax Coercion ::= "TODO"

  syntax Expr     ::= "var"  "(" Var ")"
                    | "lit"  "(" Lit ")"
                    | "app"  "(" App ")"
                    | "lam"  "(" Lam ")"
                    | "let"  "(" Let ")"
                    | "case" "(" Case ")"
                    | "cast" "(" Cast ")"
                    | "tick" "(" Tick ")"
                    | "type" "(" Type ")"
                    | Coercion

  syntax SeqBind  ::= Binding | Binding SeqBind

  syntax Pgm       ::= SeqBind

endmodule
