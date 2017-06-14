{-# LANGUAGE NoImplicitPrelude #-}

module Nats where

import qualified Prelude as P
import qualified ChurchNat3 as CN3

data Nat = Z | S Nat

foo = CN3.isZero

one = S Z
