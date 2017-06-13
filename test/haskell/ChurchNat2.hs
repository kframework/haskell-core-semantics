{-# LANGUAGE NoImplicitPrelude #-}
module ChurchNat2 where

zero = \s -> \z -> z
one  =  \s -> \z -> s z

succ  = \n -> \s -> \z -> s (n s z)
two   = succ one
three = succ two

plus = \n1 -> \n2 -> (\s -> \z -> n2 s (n1 s z))

pair = \f s b -> b f s

true  = \x -> \y -> x
false = \x -> \y -> y

first  = \p -> p true
second = \p -> p false

zz  = pair zero zero
ss  = \p -> pair (second p) (plus one (second p))
prd = \m -> first (m ss zz)

isZero = \m -> m (\x -> false) true
land  = \p -> \q -> p q false

minus = \n1 -> \n2 -> n1 prd n2

nine = succ (succ (succ (succ (succ (succ three)))))

result =
  let x = succ three
      y = succ (succ (three))
  in (isZero (plus x y)) 1 0
