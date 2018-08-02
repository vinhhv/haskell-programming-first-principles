module MatchTypes where

-- 1)
i :: Num a => a
-- i :: a -> doesn't work
i = 1

-- 2)
-- f :: Float
-- f :: Num a => a
-- 3)
f :: Fractional a => a
f = 1.0

-- 4)
-- a :: Float
a :: RealFrac a => a
a = 1.0
