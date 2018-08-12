module Rewrite where

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False
-- myAny f = foldr (\a b -> f a || b) False

myElemFold :: Eq a => a -> [a] -> Bool
myElemFold x = foldr ((||) . (==x)) False
-- myElemFold x = foldr (\a b -> x == a || b) False

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny x = any (==x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []
-- myMap f = foldr (\a b -> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b ->
                     if f a == True
                     then a : b
                     else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []
-- squishMap f = foldr (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
-- squishAgain = squishMap (\a -> a)

myMaximumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMaximumBy f xs = foldr (\a b ->
                        if f a b == GT
                        then a
                        else b) (last xs) xs

myMinimumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMinimumBy f xs = foldr (\a b ->
                        if f a b == LT
                        then a
                        else b) (last xs) xs
