module Exercises where

-- 1)

tensDigit :: Integral a => a -> a
-- tensDigit x = d
--    where xLast = x `div` 10
--          d     = xLast `mod` 10

tensDigit x = d
   where (xLast, _) = x `divMod` 10
         (_, d)     = xLast `divMod` 10

hunsDigit :: Integral a => a -> a
hunsDigit = tensDigit . (`div` 10)

-- 2)

foldBool :: a -> a -> Bool -> a
foldBool x y b
  | b          = x
  | otherwise  = y

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y b =
  case b of
    True  -> y
    False -> x

-- 3)

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
