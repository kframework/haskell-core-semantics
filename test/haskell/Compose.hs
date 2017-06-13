module Compose where

comp :: (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)

result = comp (\x -> x) (\y -> y)
