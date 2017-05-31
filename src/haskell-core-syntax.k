module HASKELL-CORE-COMMON

  syntax Var         ::= "tmVar" "(" Type "," Name ")"
                       | "tyVar" "(" Type "," Name ")"

  syntax Rational    ::= Int "%" Int

  // TODO: Native support for chars and rationals (char, float, double)
  syntax Lit         ::= "machChar"
                       | "machStr"           "(" String ")"
                       | "nullAddr"
                       | "machInt"           "(" Int ")"
                       | "machInt64"         "(" Int ")"
                       | "machWord"          "(" Int ")"
                       | "machWord64"        "(" Int ")"
                       | "machFloat"         "(" Rational ")"
                       | "machDouble"        "(" Rational ")"
                       | "machLabelFunSome"  "(" String "," Int ")"
                       | "machLabelDataSome" "(" String "," Int ")"
                       | "machLabelFunNone"  "(" String ")"
                       | "machLabelDataNone" "(" String ")"
                       | "litInt"            "(" Int "," Type ")"

  syntax BindingList ::= "emptyBind"
                       | "bind" "(" Var "," Expr "," BindingList ")"

  syntax Binding  ::= "nonRec" "(" Var "," Expr ")"
                    | "rec" "(" BindingList ")"

  syntax VarList  ::= "VarEmpty"
                    | "VarCons" "(" Var "," VarList ")"

  syntax HsId
  syntax Name     ::= "name" "(" HsId ")"

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
                    | LitExpr
                    | "app"  "(" Expr "," Expr ")" [strict(1)]
                    | "lam"  "(" Var "," Expr ")"
                    | "let"  "(" Binding "," Expr ")"
                    | "case" "(" Expr "," Var "," Type "," AltList ")"
                      [strict(1)]
                    | "cast" "(" Expr "," Coercion ")"
                    | "tick" "(" Tickish "," Expr ")"
                    | "type" "(" Type ")"
                    | Coercion
  syntax LitExpr ::= "lit"  "(" Lit ")"

  syntax SeqBind  ::= Binding | Binding SeqBind [klabel(bindings)]

endmodule

module HASKELL-CORE-SYNTAX

  imports HASKELL-CORE-COMMON
  syntax HsId ::=
    r"[a-zA-Z0-9\\_$#:\\[\\]'/\\-+=][a-zA-Z0-9\\_$#:\\[\\]'/\\-+=]*"
    [token]

endmodule
