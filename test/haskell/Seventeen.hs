{-# LANGUAGE NoImplicitPrelude #-}

module Seventeen where

import qualified Prelude

data Nat = S Nat | Z

plus :: Nat -> Nat -> Nat
plus Z y = y
plus (S x) y = S (plus x y)

seventeen :: Nat
seventeen = (\ x -> plus Z x) Z
