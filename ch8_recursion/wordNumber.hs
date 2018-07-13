module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "not a single digit bitch"

digits :: Int -> [Int]
digits n = go (abs n) []
  where go num ds
         | num < 10 = [num] ++ ds
         | otherwise = go (div num 10) ([mod num 10] ++ ds)

wordNumber :: Int -> String
wordNumber n = concat . (intersperse "-") . (map digitToWord) . digits $ n
