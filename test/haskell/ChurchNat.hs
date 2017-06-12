{-# LANGUAGE NoImplicitPrelude #-}
module ChurchNat where

zero = \s -> \z -> z
one  =  \s -> \z -> s z

succ  = \n -> (\s -> \z -> s (n (s z)))

two   = succ one

three = succ two
