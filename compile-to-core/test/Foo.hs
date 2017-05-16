module Foo where

one :: Integer
one = 1

foo :: Integer -> Integer
foo = let a = 2 in \x ->  one

fact 0 = 1
fact x = x * (fact (x-1))

bar :: a -> a -> a -> String
bar = let b = "tosun" in \x -> \y -> \z -> b
