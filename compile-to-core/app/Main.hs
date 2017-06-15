{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           BasicTypes            (Arity, FunctionOrData (..))
import           CoAxiom               (Branched, CoAxBranch (..), CoAxiom (..),
                                        CoAxiomRule (..), Role (..), cab_lhs,
                                        cab_rhs, co_ax_tc, fromBranches)
import           Control.Monad         ((<=<))
import           CoreSyn
import           Data.ByteString.Char8 (unpack)
import           Data.List             (intercalate)
import           Data.Semigroup        ((<>))
import           DataCon               (dataConSourceArity)
import           FastString            (unpackFS)
import           GHC
import           GHC.Paths             (libdir)
import           HscTypes              (mg_binds, mg_tcs)
import           Literal
import           Options.Applicative
import qualified Outputable            as OP
import           TyCon                 (AlgTyConRhs (..), algTyConRhs,
                                        isAlgTyCon, isPromotedDataCon,
                                        isPromotedDataCon_maybe, isTupleTyCon,
                                        tyConKind, tyConName)
import           TyCoRep               (Coercion (..), LeftOrRight (..),
                                        TyBinder (..), TyLit (..), Type (..),
                                        UnivCoProvenance (..),
                                        VisibilityFlag (..))
import           Var                   (Var, isId, isTyVar, varName, varType)

errorTODO :: a
errorTODO = error "TODO"

type ShouldOmitTypes = Bool

data Flags = Flags
  { shouldOmitTypes   :: ShouldOmitTypes
  , shouldUseColor    :: Bool
  , shouldStripResult :: Bool
  }

args :: [String] -> String
args ss = "(" ++ intercalate ", " ss ++ ")"

prVar :: Flags -> Var -> String
prVar flg e
  | isId e = "tmVar" ++ args [prType flg (varType e), prName $ varName e]
  | isTyVar e = "tyVar" ++ args [prType flg (varType e), prName $ varName e]
  | otherwise = error "this case should not happen."

prList :: String -> [String] -> String
prList t []     = t ++ "Empty"
prList t (x:xs) = (t ++ "Cons") ++ args [x, prList t xs]

prName :: Name -> String
prName n = "name" ++ args [OP.showSDocUnsafe (OP.ppr n)]

prAlt :: Flags -> Alt Var -> String
prAlt flg (ac, bs, e) =
  let acs = prAltCon flg ac
      bss = prList "Var" $ prVar flg <$> bs
      es  = prExpr flg e
  in "alt" ++ args [acs, bss, es]

prArity :: Arity -> String
prArity x = "arity" ++ args [show x]

prDataCon :: DataCon -> String
prDataCon dc =
  let arg1 = prName $ getName dc
      -- As we are ignoring types for now it is okay to just use
      -- `dataConSourceArity`. In the future, however, we might need to use
      -- `dataConOrigArgTys`, that gives the typed arity information.
      arg2 = prArity $ dataConSourceArity dc
  in "dataCon" ++ args [arg1, arg2]

prAltCon :: Flags -> AltCon -> String
prAltCon _   (DataAlt dc) = "dataAlt" ++ args [prDataCon dc]
prAltCon flg (LitAlt lit) = "litAlt" ++ args [prLit flg lit]
prAltCon _   DEFAULT      = "defaultAlt()"

prAlgTyConRhs :: AlgTyConRhs -> String
prAlgTyConRhs (DataTyCon dcs _) =
  "dataTyCon" ++ args [prList "DataCon" $ prDataCon <$> dcs]
-- The omitted information in the following case might be needed in the future.
prAlgTyConRhs (AbstractTyCon _) = "abstractTyCon()"
prAlgTyConRhs (NewTyCon dc _ _ _) = "newTyCon" ++ args [prDataCon dc]
prAlgTyConRhs _ = error "Many cases of prAlgTyConRhs not implemented yet."

prTyCon :: Flags -> TyCon -> String
prTyCon flg tc
  | isFunTyCon tc =
      "arrTyCon()"
  | isTypeSynonymTyCon tc =
      "synTyCon" ++ args [prType flg $ tyConKind tc]
  | isTupleTyCon tc =
      "tupleTyCon()" ++ args [prType flg $ tyConKind tc]
  | isAlgTyCon tc =
      let arg1 = prName $ tyConName tc
          arg2 = prType flg $ tyConKind tc
          arg3 = prAlgTyConRhs $ algTyConRhs tc
      in "algTyCon" ++ args [arg1, arg2, arg3]
  | isPrimTyCon tc = "primTyCon" ++ args [prName $ tyConName tc]
  | isPromotedDataCon tc =
      case isPromotedDataCon_maybe tc of
        Just tc' -> "promDataCon" ++ args [prDataCon tc']
        Nothing  -> error "there should be a dataCon"
  | otherwise = error "InternalError: this case must not have happened."

prRole :: Role -> String
prRole Nominal          = "nom"
prRole Representational = "repr"
prRole Phantom          = "phant"

prCoAxBranch :: Flags -> CoAxBranch -> String
prCoAxBranch flg cab =
  let
    tvs     = prList "tyVar" $ prVar flg <$> cab_tvs cab
    roles   = prList "role" $ prRole <$> cab_roles cab
    lhs_str = args $ prType flg <$> cab_lhs cab
    rhs_str = prType flg $ cab_rhs cab
  in
    "coAxBranch" ++ args [tvs, roles, lhs_str, rhs_str]

prCoAxiom :: Flags -> CoAxiom Branched -> String
prCoAxiom flg ca =
    let
      t            = prTyCon flg $ co_ax_tc ca
      rho          = prRole $ co_ax_role ca
      branches     =  fromBranches (co_ax_branches ca)
      axBranchList = prList "CoAxBranch" $ prCoAxBranch flg <$> branches
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

prCoercion :: Flags -> Coercion -> String
prCoercion flg (Refl r ty) = "refl" ++ args [prRole r, prType flg ty]
prCoercion flg (TyConAppCo role tc cs) =
  let csArg = prList "coercion" (prCoercion flg <$> cs)
  in "tyConAppCo" ++ args [prRole role, prTyCon flg tc, csArg]
prCoercion flg (AppCo coe1 coe2) =
  "appCo" ++ args (prCoercion flg <$> [coe1, coe2])
-- TODO: Make sure that this is what we want for the `CoVarCo` case.
prCoercion flg (CoVarCo x) = "coVarCo" ++ args [prVar flg x]
prCoercion flg (AxiomInstCo cab bi cs) =
  let csArgs = prList "coercion" $ prCoercion flg <$> cs
      biArg  = "brIndex" ++ args [show bi]
  in "axiomInstCo" ++ args [prCoAxiom flg cab, biArg, csArgs]
prCoercion flg (UnivCo prov r ty1 ty2)  =
  let arg1 = prProvenance prov
      arg2 = prType flg ty1
      arg3 = prType flg ty2
      arg4 = prRole r
  in "univCo" ++ args [arg1, arg2, arg3, arg4]
prCoercion flg (SymCo co) =
  "symCo" ++ args [prCoercion flg co]
prCoercion flg (TransCo co1 co2) =
  "transCo" ++ args [prCoercion flg co1, prCoercion flg co2]
prCoercion flg (AxiomRuleCo car cs) =
  "axiomRuleCo" ++ args (prCoAxiomRule car : (prCoercion flg <$> cs))
prCoercion flg (NthCo i co) =
  "nthCo" ++ args [show i, prCoercion flg co]
prCoercion flg (LRCo CLeft co) =
  "leftProjCo" ++ args [prCoercion flg co]
prCoercion flg (LRCo CRight co) =
  "rightProjCo" ++ args [prCoercion flg co]
prCoercion flg (InstCo co1 co2) =
  "instCo" ++ args [prCoercion flg co1, prCoercion flg co2]
prCoercion flg (CoherenceCo co kco) =
  "coherenceCo" ++ args [prCoercion flg co, prCoercion flg kco]
prCoercion flg (KindCo co) =
  "kindCo" ++ args [prCoercion flg co]
prCoercion flg (SubCo co) =
  "subCo" ++ args [prCoercion flg co]
prCoercion flg (ForAllCo tv kc c) =
  "forAllCo" ++ args [prVar flg tv, prCoercion flg kc, prCoercion flg c]

prVisibilityFlag :: VisibilityFlag -> String
prVisibilityFlag Visible   = "visible"
prVisibilityFlag Specified = "specified"
prVisibilityFlag Invisible = "invisible"

prType :: Flags -> Type -> String
prType flg ty'
  | shouldOmitTypes flg && shouldUseColor flg = "\x1b[31m[type omitted]\x1b[0m"
  | shouldOmitTypes flg = "[type omitted]"
  | otherwise =
      case ty' of
        TyVarTy x -> prVar flg x
        AppTy ty1 ty2 ->
          "appTy" ++ args [prType flg ty1 , prType flg ty2]
        TyConApp tc kt ->
          "tyConApp" ++ args (prTyCon flg tc : (prType flg <$> kt))
        ForAllTy (Named tyvar vf) ty ->
          "forallTy" ++ args [prVar flg tyvar, prVisibilityFlag vf, prType flg ty]
        ForAllTy (Anon ty1) ty2 ->
          "arr" ++ args [prType flg ty1, prType flg ty2]
        LitTy tyl -> prTyLit tyl
        CastTy ty kindco ->
          "castTy" ++ args [prType flg ty, prCoercion flg kindco]
        CoercionTy co ->
          "coercionTy" ++ args [prCoercion flg co]

-- TODO: Get this into KORE format.
prLit :: Flags -> Literal -> String
prLit _   (MachChar c) = "machChar" ++ args [[c]]
prLit _   (MachStr bs) = "machStr" ++ args ["\"" ++ unpack bs ++ "\""]
prLit _   MachNullAddr = "nullAddr"
prLit _   (MachInt n) = "machInt" ++ args [show n]
prLit _   (MachInt64 n) = "machInt64" ++ args [show n]
prLit _   (MachWord n) = "machWord" ++ args [show n]
prLit _   (MachWord64 n) = "machWord64" ++ args [show n]
prLit _   (MachFloat r) = "machFloat" ++ args [show r]
prLit _   (MachDouble r) = "machDouble" ++ args [show r]
-- TODO: It might be nice to consider an alternative way of handling
-- instead of having separate operators `Maybe Integer`.
prLit _   (MachLabel fs (Just n) IsFunction) =
  "machLabelFunSome" ++ args [unpackFS fs, show n]
prLit _   (MachLabel fs (Just n) IsData) =
  "machLabelDataSome" ++ args [unpackFS fs, show n]
prLit _   (MachLabel fs Nothing IsFunction) =
  "machLabelFunNone" ++ args [unpackFS fs]
prLit _   (MachLabel fs Nothing IsData) =
  "machLabelDataNone" ++ args [unpackFS fs]
prLit flg (LitInteger n ty) = "litInt" ++ args [show n, prType flg ty]

prTyLit :: TyLit -> String
prTyLit (NumTyLit n)  = "numTyLit" ++ args [show n]
prTyLit (StrTyLit fs) = "strTyLit" ++ args [unpackFS fs]

prBinding :: Flags -> Bind CoreBndr -> String
prBinding flg (NonRec b e) =
  let definiendum = prVar flg b in
    case definiendum of
      "tmVar([type omitted], name(result))" | shouldStripResult flg
        -> prExpr flg e
      _ ->  "nonRec" ++ args [definiendum, prExpr flg e]
prBinding flg (Rec bs) =
  let prBindingList [] = "emptyBind"
      prBindingList ((b, e):bs') =
        let arglist = [prVar flg b, prExpr flg e, prBindingList bs']
        in "bind" ++ args arglist
  in "rec" ++ args [prBindingList bs]

prTickish :: Tickish Id -> String
-- TODO: Figure out what these `Tickish` constructors are about.
prTickish ProfNote    {} = "profNote()"
prTickish HpcTick     {} = "hpcTick()"
prTickish Breakpoint  {} = "breakpoint()"
prTickish SourceNote  {} = "sourceNote()"

prExpr :: Flags -> CoreExpr -> String
prExpr flg (Var x) = "var" ++ args [prVar flg x]
prExpr flg (Lit a) = "lit" ++ args [prLit flg a]
prExpr flg (App e1 e2) = "app" ++ args (prExpr flg <$> [e1, e2])
-- TODO: Figure out how to print `CoreBndr`
prExpr flg (Lam x e) = "lam" ++ args [prVar flg x, prExpr flg e]
prExpr flg (Let b e) = "let" ++ args [prBinding flg b, prExpr flg e]
prExpr flg (Case e b ty alts)  =
  let altsStr = prList "alt" $ prAlt flg <$> alts
  in "case" ++ args [prExpr flg e, prVar flg b, prType flg ty, altsStr]
prExpr flg (Cast e co) = "cast" ++ args [prExpr flg e, prCoercion flg co]
prExpr flg (Tick tid e) = "tick" ++ args [prTickish tid, prExpr flg e]
prExpr flg (Type ty) = "type" ++ args [prType flg ty]
prExpr flg (Coercion co) = "coerce" ++ args [prCoercion flg co]

getCoreBinds :: DesugaredModule -> IO [CoreBind]
getCoreBinds dsm = return $ mg_binds . coreModule $ dsm

getTyCons :: DesugaredModule -> IO [TyCon]
getTyCons dsm = return $ mg_tcs . coreModule $ dsm

getDesugaredModule :: String -> IO DesugaredModule
getDesugaredModule modName = runGhc (Just libdir) $ do
    _ <- setSessionDynFlags =<< getSessionDynFlags
    target <- guessTarget (modName ++ ".hs") Nothing
    setTargets [target]
    _ <- load LoadAllTargets
    desugarModule <=< typecheckModule
                  <=< parseModule
                  <=< getModSummary $ mkModuleName modName

data Args = Args
  { moduleName  :: String
  , noTypes     :: Bool
  , stripResult :: Bool
  , colorful    :: Bool
  , outFile     :: Maybe String }

argParse :: Parser Args
argParse = Args
        <$> argument str (metavar "MODULE")
        <*> switch (long "no-types" <> help "Omit type information")
        <*> switch (  long "strip-result"
                   <> help "If there is a top-level declaration of a \
                            \definiendum named `result`, strip its definiens \
                            \out as a naked expression.")
        <*> switch (long "color" <> short 'c' <> help "Enable colorful output")
        <*> optional (strOption
              (  long "output-file"
              <> short 'o'
              <> help "File to dump output in"
              <> metavar "OUTFILE"))

runWithArgs :: Args -> IO ()
runWithArgs (Args mn nt srd clr mybfname) = do
  let flg = Flags nt clr srd
  dsm <- getDesugaredModule mn
  cbs <- getCoreBinds dsm
  tcs <- getTyCons dsm
  let tcsStr      = intercalate "\n\n" (prTyCon flg <$> tcs)
  let bindingsStr = intercalate "\n\n" (prBinding flg <$> cbs)
  let output      = tcsStr ++ "\n\n" ++ bindingsStr
  case mybfname of
    Just fname -> writeFile fname output
    Nothing    -> putStrLn output

main :: IO ()
main = do
  let pdStr  = "Compile Haskell to KORE representation of GHC Core"
  let hdrStr = "compile-to-core - Compile GHC Core to KORE"
  let opts = info (argParse <**> helper) (fullDesc <> progDesc pdStr <> header hdrStr)
  runWithArgs =<< execParser opts
