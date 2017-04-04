module HASKELL-CORE-SYNTAX

  syntax Module ::= "%module" Id ListTdef ListVdefg

  syntax Tdef ::= "%data" QualIdent ListTbind "=" "{" ListCdef "}"
                | "%newtype" QualIdent ListTbind MaybeTy

  syntax ListTdef ::= ""
                    | Tdef ";" ListTdef

  syntax MaybeTy ::= "=" Ty
                   | ""

  syntax Cdef ::= QualIdent ListATbind ListTyt

  syntax ListTyt ::= ""
                   | Tyt ListTyt

  syntax Tyt ::= Ty2

  syntax ListCdef ::= Cdef
                    | Cdef ";" ListCdef

  syntax Vdefg ::= "%rec" "{" ListVdef "}"
                 | Vdef

  syntax ListVdefg ::= ""
                     | Vdefg ";" ListVdefg

  syntax Vdef ::= QualIdent "::" Ty "=" Exp
                | Id "::" Ty "=" Exp

  syntax ListVdef ::= Vdef
                    | Vdef ";" ListVdef

  syntax Exp2 ::= Id
                | QualIdent
                | Lit
                | "(" Exp ")" [bracket]

  syntax Exp1 ::= Exp1 Exp2
                | Exp2 "@" Ty2
                | Exp2

  syntax Exp ::= "\\" ListBind "->" Exp
               | "%let" Vdefg "in" Exp
               | "%case" Exp2 "of" Vbind "{" ListAlt "}"
               | "%coerce" Ty2  Exp
               | "%note" String Exp
               | "%external" String Ty
               | Exp1

  syntax Bind ::= Vbind
                | "@" Tbind

  syntax ListBind ::= Bind
                    | Bind ListBind

  syntax Alt ::= QualIdent ListATbind ListVbind "->" Exp
               | Lit "->" Exp
               | "%_" "->" Exp

  syntax ListAlt ::= Alt
                   | Alt ";" ListAlt

  syntax Vbind ::= "(" Id "::" Ty ")"


  syntax ListVbind ::= ""
                     | Vbind ListVbind

  syntax Tbind ::= ""
                 | Vbind ListVbind

  syntax Tbind ::= "(" Id "::" Kind1 ")"
                 | Id

  syntax ListTbind ::= ""
                     | Tbind ListTbind

  syntax ATbind ::= "@" Tbind

  syntax ListATbind ::= ""
                      | ATbind ListATbind

  syntax Ty2 ::= Id
               | QualIdent
               | "(" Ty ")" [bracket]

  syntax Ty1 ::= Ty1 Ty2
               | Ty2

  syntax Ty ::= Ty1 "->" Ty2
              | "%forall" ListTbind "." Ty
              | Ty1

  syntax Kind1 ::= "*"
          | "#"
          | "?"
          | "(" Kind ")"    [bracket]

  syntax Kind ::= Kind1 "->" Kind
         | Kind1

  syntax Lit ::= "(" Int "::" Ty2 ")"
        // | "(" Double "::" Ty2 ")"
        // | "(" Char "::" Ty2 ")"
        | "(" String "::" Ty2 ")"

  syntax QualIdent ::= Id "." Id

endmodule
