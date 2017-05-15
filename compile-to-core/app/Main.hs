{-# LANGUAGE ExplicitForAll     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax      #-}

module Main where

import qualified BasicTypes
import           Control.Monad      ((<=<))
import           CoreSyn
import           Data.List          (concat, intersperse)
import           DynFlags           (defaultLogAction)
import           GHC
import           GHC.Paths          (libdir)
import           HscTypes           (mg_binds)
import           Literal
import qualified Name
import           System.Environment
import qualified TyCoRep
import qualified Outputable as OP
import           Var

deriving instance Show BasicTypes.FunctionOrData
deriving instance Show Literal

foo :: GhcMonad m => FilePath -> m CoreModule
foo = GHC.compileToCoreModule

args :: [Expr CoreBndr] -> String
args xs = concat $ intersperse "," (map prettyExpr xs)

prettyVar :: Var -> String
prettyVar x = Name.nameStableString $ getName  x

prettyExpr :: Expr CoreBndr -> String
prettyExpr (Var v)            = error "TODO"
prettyExpr (Lit l)            = show l
prettyExpr (Type ty)          = error "TODO"
prettyExpr (Coercion co)      = error "TODO"
prettyExpr (App e1 e2)        = error "TODO"
prettyExpr (Lam _ e)          = error "TODO"
prettyExpr (Let bind body)    = error "TODO"
prettyExpr (Case e _ ty alts) = error "TODO"
prettyExpr (Tick t e)         = error "TODO"
prettyExpr (Cast e co)        = error "TODO"

prettyBind :: CoreBind -> String
prettyBind (NonRec _ e) = "nonrec" ++ (args [e])

compileToCore :: String -> IO [CoreBind]
compileToCore modName = runGhc (Just libdir) $ do
    setSessionDynFlags =<< getSessionDynFlags
    target <- guessTarget (modName ++ ".hs") Nothing
    setTargets [target]
    load LoadAllTargets
    ds <- desugarModule <=< typecheckModule <=< parseModule <=< getModSummary $ mkModuleName modName
    return $ mg_binds . coreModule $ ds

main = do
  c <- compileToCore "x = 5"
  mapM_ (\x -> putStrLn (prettyBind x)) c
