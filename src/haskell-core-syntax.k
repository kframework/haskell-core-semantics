module HASKELL-CORE-SYNTAX

  syntax Var         ::= "tmVar" "(" Type "," CoreBndr ")"
                       | "tyVar" "(" Type "," CoreBndr ")"

  // TODO: Native support for chars and rationals (char, float, double)
  syntax Lit         ::= "machChar"
                       | "machStr"           "(" String ")"
                       | "nullAddr"
                       | "machInt"           "(" Int ")"
                       | "machInt64"         "(" Int ")"
                       | "machWord"          "(" Int ")"
                       | "machWord64"        "(" Int ")"
                       | "machFloat"
                       | "machDouble"
                       | "machLabelFunSome"  "(" String "," Int ")"
                       | "machLabelDataSome" "(" String "," Int ")"
                       | "machLabelFunNone"  "(" String ")"
                       | "machLabelDataNone" "(" String ")"
                       | "litInt"            "(" Int "," Type ")"

  syntax CoreBndr    ::= "coreBndr" "(" Id ")"

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

  syntax Tickish  ::= "profNote"    "(" ")"
                    | "hpcTick"     "(" ")"
                    | "breakpoint"  "(" ")"
                    | "sourceNote"  "(" ")"

  /* TODO: type system will eventually be implemented. */
  syntax Type     ::= "<type>"

  syntax Coercion ::= "TODO"

  // TODO: the "lit" case here should be a KResult
  syntax Expr     ::= "var"  "(" Var ")"
                    | "lit"  "(" Lit ")"
                    | "app"  "(" Expr "," Expr ")"
                    | "lam"  "(" CoreBndr "," Expr ")"
                    | "let"  "(" Binding "," Expr ")"
                    | "case" "(" Expr "," Var "," Type "," AltList ")" [strict(1)]
                    | "cast" "(" Expr "," Coercion ")"
                    | "tick" "(" Tickish "," Expr ")"
                    | "type" "(" Type ")"
                    | Coercion

  syntax SeqBind  ::= Binding | Binding SeqBind

  syntax Pgm       ::= SeqBind

endmodule
