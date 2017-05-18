{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           BasicTypes            (FunctionOrData (..))
import qualified BasicTypes
import           CoAxiom               (Branched, CoAxBranch (..), CoAxiom (..),
                                        CoAxiomRule (..), Role (..), cab_lhs,
                                        cab_rhs, co_ax_tc, fromBranches)
import           Control.Monad         ((<=<))
import           CoreSyn
import           Data.ByteString.Char8 (unpack)
import           Data.List             (concat, intercalate)
import           DynFlags              (defaultLogAction, ghcMode)
import           FastString            (unpackFS)
import           GHC
import           GHC.Paths             (libdir)
import           HscTypes              (mg_binds)
import           Literal
import qualified Name
import qualified Outputable            as OP
import           System.Environment
import           TyCon
import           TyCoRep               (Coercion (..), KindCoercion (..),
                                        LeftOrRight (..), TyBinder (..),
                                        Type (..), UnivCoProvenance (..))
import qualified Unique                as U
import           Var                   (Var, isId, isTyVar, varName, varType)

args :: [String] -> String
args ss = "(" ++ intercalate ", " ss ++ ")"

outVar :: CoreBndr -> String
outVar = show . U.getUnique

prVar :: Var -> String
prVar e
  | isTyVar e = "tyVar" ++ args [prType (varType e), outVar e]
  | isId e = "tmVar" ++ args [prType (varType e), outVar e]

prList :: String -> [String] -> String
prList t ss = t ++ ">>>" ++ intercalate "::" ss

prTyCon :: TyCon -> String
prTyCon tc
  | isFunTyCon tc = "arrTyCon"
  | isAlgTyCon tc = "algTyCon" ++ "{" ++ prType (tyConKind tc) ++ "}"
  | isTypeSynonymTyCon tc = "synTyCon" ++ "{" ++ prType (tyConKind tc) ++ "}"
  | isTupleTyCon tc = "tupleTyCon" ++ "{" ++ prType (tyConKind tc) ++ "}"
  | isPrimTyCon tc = "primTyCon()"
  | isPromotedDataCon tc = "promDataCon()"

prRole :: Role -> String
prRole Nominal          = "nom"
prRole Representational = "repr"
prRole Phantom          = "phant"

prCoAxBranch :: CoAxBranch -> String
prCoAxBranch cab =
  let
    vars    = cab_tvs cab
    lhs_str = prType $ cab_rhs cab
    rhs_str = args $ prType <$> cab_lhs cab
    _       = error "TODO"
  in
    "coAxBranch" ++ args [lhs_str, rhs_str]

prCoAxiom :: CoAxiom Branched -> String
prCoAxiom ca =
    let
      t            = prTyCon $ co_ax_tc ca
      rho          = prRole $ co_ax_role ca
      branches     =  fromBranches (co_ax_branches ca)
      axBranchList = prList "CoAxBranch" $ prCoAxBranch <$> branches
    in
      "coAxiom" ++ args [t, rho, axBranchList]

prProvenance :: UnivCoProvenance -> String
prProvenance UnsafeCoerceProv   = "unsafeProv"
prProvenance (PhantomProv _)    = "phantProv"
prProvenance (ProofIrrelProv _) = "proofIrrelProv"

prCoAxiomRule :: CoAxiomRule -> String
prCoAxiomRule car = unpackFS $ coaxrName car

prCoercion :: Coercion -> String
prCoercion (Refl r ty) = "refl()"
prCoercion (TyConAppCo role tc cs) =
  let
    csArg = prList "Coercion" (prCoercion <$> cs)
  in
    "tyConAppCo" ++ args (prRole role : prTyCon tc : (prCoercion <$> cs))
prCoercion (AppCo coe1 coe2) =
  "appCo" ++ args (prCoercion <$> [coe1, coe2])
prCoercion (CoVarCo v) =
  outVar v
-- TODO: Complete.
prCoercion (AxiomInstCo cab bi cs) =
  "axiomInstCo" ++ args [prCoAxiom cab, error "TODO"]
prCoercion (UnivCo prov r ty1 ty2)  =
  "univCo" ++ args [prProvenance prov, prType ty1, prType ty2, prRole r]
prCoercion (SymCo co) =
  "symCo" ++ args [prCoercion co]
prCoercion (TransCo co1 co2) =
  "transCo" ++ args [prCoercion co1, prCoercion co2]
prCoercion (AxiomRuleCo car cs) =
  "axiomRuleCo" ++ args (prCoercion <$> cs)
prCoercion (NthCo i co) =
  "nthCo" ++ args [show i, prCoercion co]
prCoercion (LRCo CLeft co) =
  "leftProjCo" ++ args [prCoercion co]
prCoercion (LRCo CRight co) =
  "rightProjCo" ++ args [prCoercion co]
prCoercion (InstCo co1 co2) =
  "instCo" ++ args [prCoercion co1, prCoercion co2]
prCoercion (CoherenceCo co kco) =
  "coherenceCo" ++ args [prCoercion co, prCoercion kco]
prCoercion (KindCo co) =
  "kindCo" ++ args [prCoercion co]
prCoercion (SubCo co) =
  "subCo" ++ args [prCoercion co]

prType :: Type -> String
prType (TyVarTy x) =
  outVar x
prType (AppTy ty1 ty2)  =
  "appTy" ++ args [prType ty1, prType ty2]
prType (TyConApp tc kt) =
  "tyConApp" ++ args [prList "type" (prType <$> kt)]
prType (ForAllTy (Named tyvar vf) ty) =
  "forallTy" ++ args [prVar tyvar, prType ty]
prType (ForAllTy (Anon ty1) ty2) =
  "arr" ++ args [prType ty1, prType ty2]
prType (LitTy tyl) =
  OP.showSDocUnsafe (OP.ppr tyl)
prType (CastTy ty kindco) =
  "castTy" ++ args [prType ty, prCoercion kindco]
prType (CoercionTy co) =
  "coercionTy" ++ args [prCoercion co]

prLit :: Literal -> String
prLit (MachChar c)          = "machChar" ++ args [[c]]
prLit (MachStr bs)          = "machStr" ++ args [unpack bs]
prLit MachNullAddr          = "nullAddr"
prLit (MachInt n)           = "machInt" ++ args [show n]
prLit (MachInt64 n)         = "machInt64" ++ args [show n]
prLit (MachWord n)          = "machWord" ++ args [show n]
prLit (MachWord64 n)        = "machWord64" ++ args [show n]
prLit (MachFloat r)         = "machFloat" ++ args [show r]
prLit (MachDouble r)        = "machDouble" ++ args [show r]
-- TODO: It might be nice to consider an alternative way of handling
-- instead of having separate operators `Maybe Integer`.
prLit (MachLabel fs (Just n) IsFunction) =
  "machLabelFunSome" ++ args [unpackFS fs, show n]
prLit (MachLabel fs (Just n) IsData) =
  "machLabelDataSome" ++ args [unpackFS fs, show n]
prLit (MachLabel fs Nothing IsFunction) =
  "machLabelFunNone" ++ args [unpackFS fs]
prLit (MachLabel fs Nothing IsData) =
  "machLabelDataNone" ++ args [unpackFS fs]
prLit (LitInteger n ty) = "litInt" ++ args [show n, prType ty]

prExpr :: CoreExpr -> String
prExpr v@(Var x) = prVar x
prExpr l@(Lit a) = "lit" ++ "[" ++ OP.showSDocUnsafe (OP.ppr l) ++ "]"
prExpr (App e1 e2) = "app" ++ args (prExpr <$> [e1, e2])
prExpr (Lam x e) = "lam" ++ args [show (U.getUnique x) ++ "." ++ prExpr e]
prExpr (Let (Rec []) e2) = prExpr e2
prExpr (Let (Rec ((b, e1):bs)) e2) =
  let rest = prExpr (Let (Rec bs) e2) in
    "letrec" ++ args [prExpr e1, outVar b ++ "." ++ rest]
prExpr (Let (NonRec b e1) e2) =
    "let" ++ args [prExpr e1, (show . U.getUnique $ b) ++ "." ++ prExpr e2]
prExpr (Case e b ty alts)  =
    "case" ++ args [prExpr e, outVar b, prType ty, "altsTODO"]
prExpr (Cast e co) = "coerce" ++ args [prExpr e]
prExpr (Tick t e) = error "TODO: Tick case of prExpr."
prExpr (Type ty) = prType ty
prExpr (Coercion co) = error "TODO: Coercion case of prExpr."

prettyDecl :: (CoreBndr, Expr CoreBndr) -> String
prettyDecl (b, e) = outVar b ++ "." ++ prExpr e

prBind :: CoreBind -> String
prBind (NonRec x e) = "decl" ++ args [show (U.getUnique x), prExpr e]
prBind (Rec bs)     = "declRec" ++ args (prettyDecl <$> bs)

compileToCore :: String -> IO [CoreBind]
compileToCore modName = runGhc (Just libdir) $ do
    setSessionDynFlags =<< getSessionDynFlags
    target <- guessTarget (modName ++ ".hs") Nothing
    setTargets [target]
    load LoadAllTargets
    ds <- desugarModule <=< typecheckModule <=< parseModule <=< getModSummary $ mkModuleName modName
    return $ mg_binds . coreModule $ ds

main :: IO ()
main = do
  args <- getArgs
  c <- compileToCore (head args)
  mapM_ (putStrLn . prBind) c
