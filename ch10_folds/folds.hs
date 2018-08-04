module Folds where

import Data.Char

ex1 = foldr (*) 1 [1..5] == foldl (*) 1 [1..5]
   && foldr (*) 1 [1..5] == foldl (flip (*)) 1 [1..5]

-- foldl (flip (*)) 1 [1..3]
-- (3 * (2 * (1 * 1)))

ex5a = foldr (++) "" ["woot", "WOOT", "woot"]
ex5b = foldr max ' ' "fear is the little death"
ex5c = foldr (&&) True [False, True]
ex5d = foldr (||) True [False, True]
ex5e = foldl (\x y -> x ++ show y) "" [1..5]
ex5f = foldr const (ord 'a') [1..5]
ex5g = foldr const '0' "tacos"
ex5h = foldl (flip const) '0' "burritos"
ex5i = foldl (flip const) (ord 'z') [1..5]
