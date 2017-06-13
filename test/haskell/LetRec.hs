module LetRec where


result =
  let foo = bar
      bar = foo
  in foo
