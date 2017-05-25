module Bool where

data NBool = NTrue | NFalse deriving Show

foo :: NBool
foo = NTrue

neg :: NBool -> NBool
neg NTrue  = NFalse
neg NFalse = NTrue
