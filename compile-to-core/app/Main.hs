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

pExpr :: CoreExpr -> String
pExpr v@(Var x) = OP.showSDocUnsafe (OP.ppr v)
pExpr l@(Lit a) = OP.showSDocUnsafe (OP.ppr l)
pExpr (App e1 e2) = "app" ++ args (pExpr <$> [e1, e2])
pExpr (Lam x e) = "lam" ++ args [show (U.getUnique x) ++ "." ++ pExpr e]
pExpr (Let (NonRec b e1) e2) =
    "let" ++ args [(show . U.getUnique $ b) ++ "." ++ pExpr e1] ++ pExpr e2
pExpr (Cast e co) = "coerce" ++ args [pExpr e]
pExpr (Tick t e) = "tick" ++ args [pExpr e]
pExpr (Type _) = "type"
pExpr (Coercion co) = "coercion"

prettyBind :: CoreBind -> String
prettyBind (NonRec _ e) = "decl" ++ args [pExpr e]
prettyBind (Rec []) = ""
prettyBind (Rec ((b, e):bs)) = pExpr e

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
