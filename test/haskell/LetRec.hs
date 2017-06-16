module LetRec where

-- result :: Integer
-- result = let f x' = (\x -> x) (f x') in f 1

result :: a
result =
  let foo x = bar x
      bar y = foo y
  in foo 1
