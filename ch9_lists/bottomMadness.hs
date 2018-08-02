module BottomMadness where

import Data.Bool

-- Will they evaluate?

val1 = [x^y | x <- [1..5], y <- [2, undefined]] -- no
val2 = take 1 [x^y | x <- [1..5], y <- [2, undefined]] -- yes
val3 = sum [1, undefined, 3] -- no
val4 = length [1, 2, undefined] -- yes
val5 = length $ [1, 2, 3] ++ undefined -- no
val6 = take 1 $ filter even [1, 2, 3, undefined] -- yes
val7 = take 1 $ filter even [1, 3, undefined] -- no
val8 = take 1 $ filter odd [1, 3, undefined] -- yes
val9 = take 2 $ filter odd [1, 3, undefined] -- yes
val10= take 3 $ filter odd [1, 3, undefined] -- no

-- NF, WHNF, or neither?
-- 1) [1, 2, 3, 4, 5] -> NF
-- 2) 1 : 2 : 3 : 4 : _ -> WHNF
-- 3) enumFromTo 1 10 -> neither
-- 4) length [1, 2, 3, 4, 5] -> WHNF (neither)
-- 5) sum (enumFromTo 1 10) -> NF (neither)
-- 6) ['a'..'m'] ++ ['n'..'z'] -> neither 
-- 7) (_, 'b') -> WHNF

m1 = take 1 $ map (+1) [undefined, 2, 3] -- no
m2 = take 1 $ map (+1) [1, undefined, 3] -- yes
m3 = take 2 $ map (+1) [1, undefined, 3] -- no

itIsMystery xs = map (\x -> elem x "aeiou") xs

m = map (\x -> bool x (-x) (x == 3)) [1..10]
