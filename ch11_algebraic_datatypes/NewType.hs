{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NewType where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

--instance TooMany Goats where
--  tooMany (Goats n) = tooMany n

--
-- instance TooMany Goats where
--   tooMany (Goats n) = n > 43

-- type Numbah = Int
-- instance TooMany Numbah where
--   tooMany n = n > 43
