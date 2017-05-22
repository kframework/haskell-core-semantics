{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           BasicTypes            (FunctionOrData (..))
import           CoAxiom               (Branched, CoAxBranch (..), CoAxiom (..),
                                        CoAxiomRule (..), Role (..), cab_lhs,
                                        cab_rhs, co_ax_tc, fromBranches)
import           Control.Monad         ((<=<))
import           CoreSyn
import           Data.ByteString.Char8 (unpack)
import           Data.List             (intercalate)
import           Data.Semigroup        ((<>))
import           FastString            (unpackFS)
import           GHC
import           GHC.Paths             (libdir)
import           HscTypes              (mg_binds)
import           Literal
import           Options.Applicative
import qualified Outputable            as OP
import           TyCon                 (isAlgTyCon, isPromotedDataCon,
                                        isPromotedDataCon_maybe, isTupleTyCon,
                                        tyConKind, tyConName)
import           TyCoRep               (Coercion (..), LeftOrRight (..),
                                        TyBinder (..), TyLit (..), Type (..),
                                        UnivCoProvenance (..),
                                        VisibilityFlag (..))
import qualified Unique                as U
import           Var                   (Var, isId, isTyVar, varType)

errorTODO :: a
errorTODO = error "TODO"

type ShouldShowType = Bool

newtype Flags = Flags
  { sst :: ShouldShowType }

args :: [String] -> String
args ss = "(" ++ intercalate ", " ss ++ ")"

prVar :: Var -> String
prVar e =
  let
    outVar :: CoreBndr -> String
    outVar = show . U.getUnique
  in
    if isTyVar e
    then "tyVar" ++ args [prType (Flags True) (varType e), outVar e]
    else
      if isId e
      then "tmVar" ++ args [prType (Flags True) (varType e), outVar e]
      else error "This case should not happen."

prList :: String -> [String] -> String
prList t =
  foldr (\_ -> (\x y -> x ++ "(" ++ y ++ ")") (t ++ "Cons")) (t ++ "Empty")

prName :: Name -> String
prName n = OP.showSDocUnsafe (OP.ppr n)

prAlt :: Alt Var -> String
prAlt (ac, bs, e) =
  let acs = prAltCon ac
      bss = prList "Var" $ prVar <$> bs
      es  = prExpr e
  in "alt" ++ args [acs, bss, es]

prDataCon :: DataCon -> String
prDataCon dc = "dataCon" ++ args [prName $ getName dc]

prAltCon :: AltCon -> String
prAltCon (DataAlt dc) = "dataAlt" ++ args [prDataCon dc]
prAltCon (LitAlt lit) = "litAlt" ++ args [prLit lit]
prAltCon DEFAULT      = "defaultAlt()"

prPrimTyCon :: TyCon -> String
prPrimTyCon tc = prName $ tyConName tc

prTyCon :: TyCon -> String
prTyCon tc
  | isFunTyCon tc =
      "arrTyCon()"
  | isTypeSynonymTyCon tc =
      "synTyCon" ++ args [prType (Flags True) $ tyConKind tc]
  | isTupleTyCon tc =
      "tupleTyCon()" ++ args [prType (Flags True) $ tyConKind tc]
  | isAlgTyCon tc =
      let as = [prName (tyConName tc), prType (Flags True) $ tyConKind tc] in
      "algTyCon" ++ args as
  | isPrimTyCon tc = "primTyCon" ++ args [prPrimTyCon tc]
  | isPromotedDataCon tc =
      case isPromotedDataCon_maybe tc of
        Just tc' -> "promDataCon" ++ args [prDataCon tc']
        Nothing  -> error "there should be a dataCon"
  | otherwise = error "InternalError: this case must not have happened."

prRole :: Role -> String
prRole Nominal          = "nom"
prRole Representational = "repr"
prRole Phantom          = "phant"

prCoAxBranch :: CoAxBranch -> String
prCoAxBranch cab =
  let
    tvs     = prList "tyVar" $ prVar <$> cab_tvs cab
    roles   = prList "role" $ prRole <$> cab_roles cab
    lhs_str = args $ prType (Flags True) <$> cab_lhs cab
    rhs_str = prType (Flags True) $ cab_rhs cab
  in
    "coAxBranch" ++ args [tvs, roles, lhs_str, rhs_str]

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
prProvenance (PluginProv _)     = "pluginProv"
prProvenance (HoleProv _)       = "holeProv"

-- TODO: Make sure that this is what we want.
prCoAxiomRule :: CoAxiomRule -> String
prCoAxiomRule car = unpackFS $ coaxrName car

prCoercion :: Coercion -> String
prCoercion (Refl r ty) = "refl" ++ args [prRole r, prType (Flags True) ty]
prCoercion (TyConAppCo role tc cs) =
  let
    csArg = prList "coercion" (prCoercion <$> cs)
  in
    "tyConAppCo" ++ args (prRole role : prTyCon tc : [csArg])
prCoercion (AppCo coe1 coe2) =
  "appCo" ++ args (prCoercion <$> [coe1, coe2])
-- TODO: Make sure that this is what we want for the `CoVarCo` case.
prCoercion (CoVarCo x) = "coVarCo" ++ args [prVar x]
prCoercion (AxiomInstCo cab bi cs) =
  let
    csArgs = prList "coercion" $ prCoercion <$> cs
    biArg = "brIndex" ++ args [show bi]
  in
    "axiomInstCo" ++ args [prCoAxiom cab, biArg, csArgs]
prCoercion (UnivCo prov r ty1 ty2)  =
  let arg1 = prProvenance prov
      arg2 = prType (Flags True) ty1
      arg3 = prType (Flags True) ty2
      arg4 = prRole r
  in "univCo" ++ args [arg1, arg2, arg3, arg4]
prCoercion (SymCo co) =
  "symCo" ++ args [prCoercion co]
prCoercion (TransCo co1 co2) =
  "transCo" ++ args [prCoercion co1, prCoercion co2]
prCoercion (AxiomRuleCo car cs) =
  "axiomRuleCo" ++ args (prCoAxiomRule car : (prCoercion <$> cs))
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
prCoercion ForAllCo{} = errorTODO

prVisibilityFlag :: VisibilityFlag -> String
prVisibilityFlag Visible   = "visible"
prVisibilityFlag Specified = "specified"
prVisibilityFlag Invisible = "invisible"

prType :: Flags -> Type -> String
prType     (Flags False) _ = "[omitted]"
prType     (Flags True) (TyVarTy x) = prVar x
prType flg@(Flags True) (AppTy ty1 ty2) =
  "appTy" ++ args [prType flg ty1 , prType flg ty2]
prType flg@(Flags True) (TyConApp tc kt) =
  "tyConApp" ++ args (prTyCon tc : (prType flg <$> kt))
prType flg@(Flags True) (ForAllTy (Named tyvar vf) ty) =
  "forallTy" ++ args [prVar tyvar, prVisibilityFlag vf, prType flg ty]
prType flg@(Flags True) (ForAllTy (Anon ty1) ty2) =
  "arr" ++ args [prType flg ty1, prType flg ty2]
prType     (Flags True) (LitTy tyl) = prTyLit tyl
prType flg@(Flags True) (CastTy ty kindco) =
  "castTy" ++ args [prType flg ty, prCoercion kindco]
prType (Flags True) (CoercionTy co) =
  "coercionTy" ++ args [prCoercion co]

-- TODO: Get this into KORE format.
prLit :: Literal -> String
prLit (MachChar c) = "machChar" ++ args [[c]]
prLit (MachStr bs) = "machStr" ++ args [unpack bs]
prLit MachNullAddr = "nullAddr"
prLit (MachInt n) = "machInt" ++ args [show n]
prLit (MachInt64 n) = "machInt64" ++ args [show n]
prLit (MachWord n) = "machWord" ++ args [show n]
prLit (MachWord64 n) = "machWord64" ++ args [show n]
prLit (MachFloat r) = "machFloat" ++ args [show r]
prLit (MachDouble r) = "machDouble" ++ args [show r]
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
prLit (LitInteger n ty) = "litInt" ++ args [show n, prType (Flags True) ty]

prTyLit :: TyLit -> String
prTyLit (NumTyLit n)  = "numTyLit" ++ args [show n]
prTyLit (StrTyLit fs) = "strTyLit" ++ args [unpackFS fs]

prBinding :: Bind CoreBndr -> String
prBinding (NonRec b e) = "nonRec" ++ args [prVar b, prExpr e]
prBinding (Rec bs) =
  let prBinding' [] = "emptyBind"
      prBinding' ((b, e):bs') = "bind" ++ args [prVar b, prExpr e] ++ prBinding' bs'
  in "Rec" ++ args [prBinding' bs]

prTickish :: Tickish Id -> String
-- TODO: Figure out what these `Tickish` constructors are about.
prTickish ProfNote    {} = "profNote"
prTickish HpcTick     {} = "hpcTick"
prTickish Breakpoint  {} = "breakpoint"
prTickish SourceNote  {} = "sourceNote"

prExpr :: CoreExpr -> String
prExpr (Var x) = prVar x
prExpr (Lit a) = prLit a
prExpr (App e1 e2) = "app" ++ args (prExpr <$> [e1, e2])
prExpr (Lam x e) = "lam" ++ args [show (U.getUnique x) ++ "." ++ prExpr e]
prExpr (Let b e) = "let" ++ args [prBinding b, prExpr e]
prExpr (Case e b ty alts)  =
  let altsStr = prList "alt" $ prAlt <$> alts
  in "case" ++ args [prExpr e, prVar b, prType (Flags True) ty, altsStr]
prExpr (Cast e co) = "cast" ++ args [prExpr e, prCoercion co]
prExpr (Tick tid e) = "tick" ++ args [prTickish tid, prExpr e]
prExpr (Type ty) = prType (Flags True) ty
prExpr (Coercion co) = "coerce" ++ args [prCoercion co]

compileToCore :: String -> IO [CoreBind]
compileToCore modName = runGhc (Just libdir) $ do
    _ <- setSessionDynFlags =<< getSessionDynFlags
    target <- guessTarget (modName ++ ".hs") Nothing
    setTargets [target]
    _ <- load LoadAllTargets
    ds <- desugarModule <=< typecheckModule <=< parseModule <=< getModSummary $ mkModuleName modName
    return $ mg_binds . coreModule $ ds

data Args = Args
  { moduleName :: String
  , noTypes :: Bool
  , outFile :: Maybe String}

argParse :: Parser Args
argParse = Args
        <$> argument str (metavar "MODULE")
        <*> switch (long "no-types" <> help "Omit type information")
        <*> optional (strOption
              (  long "output-file"
              <> short 'o'
              <> help "File to dump output in"
              <> metavar "OUTFILE"))

runWithArgs :: Args -> IO ()
runWithArgs (Args mn _ (Just fname)) = do
  c <- compileToCore mn
  let output = intercalate "\n\n" (prBinding <$> c)
  writeFile fname output
runWithArgs (Args mn _ Nothing) = do
  c <- compileToCore mn
  let output = intercalate "\n\n" (prBinding <$> c)
  putStrLn output

main :: IO ()
main = do
  let pdStr  = "Compile Haskell to KORE representation of GHC Core"
  let hdrStr = "compile-to-core - Compile GHC Core to KORE"
  let opts = info (argParse <**> helper) (fullDesc <> progDesc pdStr <> header hdrStr)
  runWithArgs =<< execParser opts
