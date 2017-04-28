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

  syntax Tyt ::= Ty

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

  syntax Exp ::= Id
               | QualIdent
               | Lit
               > Exp Exp       [left]
               > "\\" ListBind "->" Exp
               > "%let" Vdefg "in" Exp
               > "%case" Exp "of" Vbind "{" ListAlt "}"
               > "%coerce" Ty  Exp
               > "%note" String Exp
               > "%external" String Ty
               > Exp "@" Ty
               > "(" Exp ")" [bracket]

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

  syntax Ty ::= Id
              > QualIdent
              > Ty "->" Ty   [right]
              > Ty Ty        [left]
              > "%forall" ListTbind "." Ty
              > "(" Ty ")"      [bracket]

  syntax Kind1 ::= "*"
          | "#"
          | "?"
          | "(" Kind ")"    [bracket]

  syntax Kind ::= Kind1 "->" Kind
         | Kind1

  syntax Lit ::= "(" Int "::" Ty ")"
        // | "(" Double "::" Ty ")"
        // | "(" Char "::" Ty ")"
        | "(" String "::" Ty ")"

  syntax QualIdent ::= Id "." Id

endmodule
