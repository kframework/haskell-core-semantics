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

  // For creating explicit HsIds in rules
  syntax HsId ::= "#parseToken" "(" String "," String ")"
                  [function, hook(STRING.string2token)]
                  
  syntax Name     ::= "name" "(" HsId ")"

  syntax Arity    ::= "arity" "(" Int ")"

  syntax DataCon  ::= "dataCon" "(" Name "," Arity ")"

  syntax DataConList ::= "DataConEmpty"
                       | "DataConCons" "(" DataCon "," DataConList ")"

  syntax AlgTyConRhs  ::= "dataTyCon" "(" DataConList ")"
                        | "abstractTyCon" "(" ")"
                        | "newTyCon" "(" DataCon ")"

  syntax TyCon        ::= "algTyCon" "(" Name "," Type "," AlgTyConRhs ")"

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
  syntax Type     ::= "[type omitted]"

  syntax Coercion ::= "TODO"

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
                    // Primitive equality
                    | "keq" "(" Expr "," Expr ")"                         [seqstrict]

  syntax LitExpr  ::= "lit"  "(" Lit ")"

  syntax Declaration ::= Binding | TyCon

  syntax SeqDecl     ::= Declaration
                       | Declaration SeqDecl                    [klabel(decls)]

  syntax Program ::= Expr | SeqDecl Expr

endmodule

module HASKELL-CORE-SYNTAX

  imports HASKELL-CORE-COMMON
  syntax HsId ::=
    r"[a-zA-Z0-9\\_$#:\\[\\]'/\\-+=&<>][a-zA-Z0-9\\_$#:\\[\\]'/\\-+=&<>]*"
    [token]

endmodule
