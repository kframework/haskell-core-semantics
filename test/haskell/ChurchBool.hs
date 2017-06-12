foo :: Integer
foo =
  let true = (\x -> \y -> x)
      false = (\x -> \y -> y)
      not p = p false true
  in (not true) 1 2
