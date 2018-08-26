module Bottoms where

data ThereYet =
  There Float Int Bool
  deriving (Eq, Show)

notYet :: Int -> Bool -> ThereYet
notYet = There 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yus :: ThereYet
yus = notQuite False
