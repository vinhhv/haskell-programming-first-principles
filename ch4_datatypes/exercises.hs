module Exercises where

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs i = if i < 0 then -i else i

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))
-- f (x1, y1) (x2, y2) = ((x2, y2), (x1, y1))

x = (+)

add1 xs = w `x` 1
     where w = length xs

myId = \x -> x

ff (a, b) = a
