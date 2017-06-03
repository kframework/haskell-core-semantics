{-# LANGUAGE NoImplicitPrelude #-}

module Sum where

import qualified Prelude as P

result :: P.Integer
result = P.foldr (P.+) 0 P.$ P.filter (\x -> 5 `P.div` x P.== 0) [1..100]
