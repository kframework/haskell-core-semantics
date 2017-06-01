{-# LANGUAGE NoImplicitPrelude #-}

module Seventeen where

import qualified Prelude

data Nat = S Nat | Z deriving Prelude.Show

plus :: Nat -> Nat -> Nat
plus Z y = y
plus (S x) y = S (plus x y)

seventeen :: Nat
seventeen = (\ x -> x) Z
