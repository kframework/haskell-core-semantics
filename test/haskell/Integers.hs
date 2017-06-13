module Integers where

x :: Int
x = 1

y :: Int
y = 2

result :: Int
result = (\_ -> \b -> b) x y
