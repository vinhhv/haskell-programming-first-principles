module MyStdLib where

myOr :: [Bool] -> Bool
myOr []     = False
myOr (h: t) = h || myOr t

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr $ map f xs

myAnyR :: (a -> Bool) -> [a] -> Bool
myAnyR f []      = False
myAnyR f (x: xs) = f x || myAnyR f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ []      = False
myElem a (x: xs) = a == x || myElem a xs

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny a xs = myAnyR (==a) xs

myReverse :: [a] -> [a]
myReverse []      = []
myReverse (x: xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish []      = []
squish (x: xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f []      = []
squishMap f (x: xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap (\x -> x)

myMaximumBy :: (a -> a -> Ordering)
            -> [a] -> a
-- myMaximumBy f (x : xs) = recurse f x xs
--   where recurse _ cur []      = cur
--         recurse fn cur (x: xs) =
--           if fn x cur == GT then recurse fn x xs else recurse fn cur xs

myMaximumBy _ (x: []) = x
myMaximumBy f (x: xs) =
  let
    y = myMaximumBy f xs
  in
    case f x y of
      LT -> y
      EQ -> x
      GT -> x


myMinimumBy _ (x: []) = x
myMinimumBy f (x: xs) =
  let
    y = myMinimumBy f xs
  in
    case f x y of
      LT -> x
      EQ -> x
      GT -> y


myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
