module LibraryFunctions where

import Data.Monoid
import Data.Foldable

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (\a b -> (a == x) || b) False
-- elem' x = getAny . foldMap (\y -> Any (y == x))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs =
  if (length xs == 0)
  then Nothing
  else Just $
    foldr (\a b -> if a < b then a else b)
          (head . toList $ xs)
          xs

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' xs =
  if (length xs == 0)
  then Nothing
  else Just $
    foldr (\a b -> if a > b then a else b)
          (head . toList $ xs)
          xs

null' :: (Foldable t) => t a -> Bool
null' = foldr (\a b -> True) False

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ b -> b + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\a b -> [a] ++ b) []
-- toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty
