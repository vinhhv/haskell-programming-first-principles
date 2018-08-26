module PityTheBool where

import Data.Int

-- 1) Cardinality 4
data BigSmall =
    Big Bool
  | Small Bool
  deriving (Eq, Show)

-- 2) Cardinality 128 + 2 = 130
data NumberOrBool =
    Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

myNumba = Numba (-128)
