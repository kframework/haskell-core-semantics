module Map where

nmap :: (a -> b) -> [a] -> [b]
nmap f []     = []
nmap  f (x:xs) = f x : nmap f xs
