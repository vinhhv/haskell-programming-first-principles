{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LogicGoats where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- Only works with GeneralizedNewtypeDeriving
newtype Goats = Goats Int deriving (Eq, Show, TooMany)

-- Only works with FlexibleInstances
-- instance TooMany (Int, String) where
--   tooMany (n, _) = n > 42

newtype Pair = Pair (Int, String) deriving (Eq, Show)

instance TooMany Pair where
  tooMany (Pair (n, _)) = n > 42

newtype TwoNums = TwoNums (Int, Int) deriving (Eq, Show)

instance TooMany TwoNums where
  tooMany (TwoNums (n, m)) = n + m > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, m) = tooMany (n + m)
