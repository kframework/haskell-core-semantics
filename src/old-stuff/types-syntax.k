module TYPES-SYNTAX

    // Defined in `types/TyCoRep.lhs` as `Type`.
    syntax Type ::= TypeVariable        // Variable
                  | TypeApplication     // Application
                  | TypeConApplication  // Application of type constructor
                  | ForAllTypeAnonymous // Function
                  | ForAllTypeNamed     // Type and coercion polymorphism
                  | KindCast            // Kind cast
                  | TypeCoercion        // Coercion used in type.

    // syntax Type ::= "TyVarTy" Id
                  // | "AppTy" Type Type
                  // | "TyConApp" Types
                  // | "ForAllTyAnon" Type Type
                  // | "ForAllTyNamed" Id Type
                  // | "CastTy" Type Coercion
                  // | "CoercionTy" Coercion

    // The names in the definition of the following sorts follow the actual
    // constructors used in `types/TyCoRep.lhs`.
    syntax TypeVariable ::= "TyVarTy" Id
    syntax TypeApplication ::= "AppTy" Type Type
    syntax TypeConApplication ::= "TyConApp" Types
    syntax ForAllTypeAnonymous ::= "ForAllTyAnon" Type Type
    syntax ForAllTypeNamed ::= "ForAllTyName" Id Type
    syntax KindCast ::= "CastTy" Type Coercion
    syntax TypeCoercion ::= "CoercionTy" Coercion

    syntax Types ::= List{Type,","}

    // As defined in `types/TyCoRep.lhs:Coercion`.
    syntax Coercion ::= // Reflexivity
                        "Refl" Role Types         // Reflexivity
                        // Type constructor application
                      | "TyConAppCo" TyCon Role Coercions
                         // Application
                      | "AppCo" Coercion Coercion
                         // Polymorphism
                      | "ForAllCo" Coercion Coercion
                         // Variable
                      | "CoVarCo" Id
                         // Axiom application
                      | "AxiomInstCo" Coercions
                      // Bunch of other stuff

    syntax Coercions ::= List{Coercion,","}

    // `UnivCo` provenance as defined in `types/TyCoRep.lhs:UnivCoProvenance`.
    syntax Prov ::= "unsafe"
                  | "phant"
                  | "irrel"

    // Roles label what equality relation a coerceion is a witness of. Nominal
    // equality means that two types are identical (have the same name);
    // representational equality means that two types have the same
    // representation; and phantom equality includes all types.
    syntax Role ::= "N" // Nominal
                  | "R" // Representational
                  | "P" // Phantom

    // Left or right deconstructor.
    // Defined in `types/TyCoRep.lhs/LeftOrRight`.
    syntax LorR ::= "left"
                  | "right"

    syntax Axiom ::= "CoAxiom" TyCon Role AxBranches

    // TODO
    // syntax AxBranch ::=

    syntax AxBranches ::= "TODO"

    syntax TyCon ::= "FunTyCon"
                     // Either AlgTyCon, TupleTyCon, SynTyCon. We are not
                     // interested in the differences between these for semantic
                     // purposes.
                   | "Exponent" Type
                   | "PrimTyCon" PrimTyCon
                   | "PromotedDataCon"

    syntax PrimTyCon ::= "intPrimTyCon"
                       | "eqPrimTyCon" TyCon TyCon      // Unboxed equality
                       | "eqReprPrimTyCon" TyCon TyCon  //
                       | "liftedTypeKindTyCon"
                       | "unliftedTypeKindTyCon"
                       | "openTypeKindTyCon"
                       | "constraintTyCon"
                       | "TYPETyCon"
                       | "LevityTyCon" Levity

    syntax Levity ::= "Lifted" | "Unlifted"

endmodule
