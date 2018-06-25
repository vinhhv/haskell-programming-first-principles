{-# LANGUAGE NoMonomorphismRestriction#-}

module DetermineTheType where

-- 1)
a1 = (* 9) 6
b1 = head [(0, "doge"),(1,"kitteh")]
c1 = head [(0 :: Integer, "Doge"),(1,"kitteh")]
d1 = if False then True else False
e1 = length [1, 2, 3, 4, 5]
f1 = (length [1, 2, 3, 4]) > (length "TACOCAT")

-- 2)
x2 = 5
y2 = x2 +5
w2 = y2 * 10

-- 3)
x3 = 5
y3 = x3 + 5
z3 y = y * 10

-- 4)
x4 = 5
y4 = x4 + 5
f4 = 4 / y4

-- 5)
x5 = "Julie"
y5 = " <3 "
z5 = "Haskell"
f5 = x5 ++ y5 ++ z5
