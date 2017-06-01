module Imports where

import Data.List (intersperse)

foo :: [String]
foo = intersperse ";" ["a", "b", "c"]
