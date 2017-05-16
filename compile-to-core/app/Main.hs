{-# LANGUAGE UnicodeSyntax      #-}

module Main where

import qualified BasicTypes
import           Control.Monad      ((<=<))
import           CoreSyn
import           Data.List          (concat, intercalate)
import           DynFlags           (defaultLogAction, ghcMode)
import           GHC
import           GHC.Paths          (libdir)
import           HscTypes           (mg_binds)
import           Literal
import qualified Name
import           System.Environment
import qualified TyCoRep
import qualified Outputable as OP
import qualified Unique as U
import           Var

args :: [String] -> String
args ss = "(" ++ intercalate "; " ss ++ ")"

prType :: Type -> String
prType = error "TODO"

pExpr :: CoreExpr -> String
pExpr v@(Var x) = show $ U.getUnique x
pExpr l@(Lit a) = "lit" ++ "[" ++ OP.showSDocUnsafe (OP.ppr l) ++ "]"
pExpr (App e1 e2) = "app" ++ args (pExpr <$> [e1, e2])
pExpr (Lam x e) = "lam" ++ args [show (U.getUnique x) ++ "." ++ pExpr e]
pExpr (Let (Rec ((b, e1):bs)) e2) =
  let rest = pExpr (Let (Rec bs) e2) in
    "letrec" ++ args [pExpr e1, outVar b ++ "." ++ rest]
pExpr (Let (NonRec b e1) e2) =
    "let" ++ args [pExpr e1, (show . U.getUnique $ b) ++ "." ++ pExpr e2]
pExpr (Case e b ty alts)  =
    "case" ++ args [pExpr e, outVar b, "tyTODO", "altsTODO"]
pExpr (Cast e co) = "coerce" ++ args [pExpr e]
pExpr (Tick t e) = "tick" ++ args [pExpr e]
pExpr (Type _) = "type"
pExpr (Coercion co) = "coercion"

outVar :: CoreBndr -> String
outVar = show . U.getUnique

prettyDecl :: (CoreBndr, Expr CoreBndr) -> String
prettyDecl (b, e) = outVar b ++ "." ++ pExpr e

prettyBind :: CoreBind -> String
prettyBind (NonRec x e) = "decl" ++ args [show (U.getUnique x), pExpr e]
prettyBind (Rec []) = ""
prettyBind (Rec bs) = "declRec" ++ args (prettyDecl <$> bs)

compileToCore :: String -> IO [CoreBind]
compileToCore modName = runGhc (Just libdir) $ do
    setSessionDynFlags =<< getSessionDynFlags
    target <- guessTarget (modName ++ ".hs") Nothing
    setTargets [target]
    load LoadAllTargets
    ds <- desugarModule <=< typecheckModule <=< parseModule <=< getModSummary $ mkModuleName modName
    return $ mg_binds . coreModule $ ds

main = do
  c <- compileToCore "Foo"
  mapM_ (putStrLn . prettyBind) c
