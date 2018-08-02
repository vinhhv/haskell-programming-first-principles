module EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool False True  = [False, True]
eftBool False False = [False]
eftBool True True   = [True]
eftBool _ _         = []

eftOrd :: Ordering
       -> Ordering
       -> [Ordering]
eftOrd LT LT = [LT]
eftOrd GT GT = [GT]
eftOrd EQ EQ = [EQ]
eftOrd LT EQ = [LT, EQ]
eftOrd EQ GT = [EQ, GT]
eftOrd LT GT = [LT, EQ, GT]
eftOrd _ _   = []

eftInt :: Int -> Int -> [Int]
eftInt from to
  | from > to = []
  | from == to = [from]
  | otherwise = from : eftInt (from + 1) to

eftChar :: Char -> Char -> [Char]
eftChar from to
  | from > to = []
  | from == to = [from]
  | otherwise = from : eftChar (succ from) to
