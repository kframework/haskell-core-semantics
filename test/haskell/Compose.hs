module Compose where

comp :: (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)
