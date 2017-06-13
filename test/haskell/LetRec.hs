module LetRec where

result :: [Integer]
result = let f x = x : f (x+1) in f 1
