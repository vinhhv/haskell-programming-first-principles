module Exercise2 where

-- Recursion

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = (count, n)
         | otherwise =
             go (n - d) d (count + 1)

-- Currying

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- Recursion

sumUp :: (Eq a, Num a) => a -> a
sumUp 0 = 0
sumUp n = n + sumUp (n - 1)

mp :: Integral a => a -> a -> a
mp mcan mplier = go mcan mplier 0
  where go mc mr total
         | mr == 0 = total
         | otherwise = go mc (mr - 1) (total + mc)
