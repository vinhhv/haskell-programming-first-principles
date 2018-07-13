module DividedBy where

data DividedResult =
    Result Integer
  | DividedByZero deriving Show

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy num denom = go (abs num) (abs denom) 0
  where go n   d count
         | n < d && (num < 0 && denom < 0) = Result count
         | n < d && (num < 0 || denom < 0)= Result (-count)
         | n < d = Result count
         | otherwise =
             go (n - d) d (count + 1)
