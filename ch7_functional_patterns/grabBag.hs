module GrabBag where

mth0 x y z = x * y * z
mth1 x y = \z -> x * y * z
mth2 x = \y -> \z -> x * y * z
mth3 = \x -> \y -> \z -> x * y * z

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x
