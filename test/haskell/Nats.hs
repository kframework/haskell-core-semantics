{-# LANGUAGE NoImplicitPrelude #-}
module Nats where

data Nat = Z | S Nat

result = S Z
