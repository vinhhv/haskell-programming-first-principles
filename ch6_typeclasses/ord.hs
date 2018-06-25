module Ords where

data DayOfWeek =
  Mon | Tue | Wed | Thu | Sat | Sun | Fri
  deriving (Eq, Ord, Show, Enum)

-- Friday is the best day
-- instance Ord DayOfWeek where
--   compare Fri Fri = EQ
--   compare Fri _   = GT
--   compare _ Fri   = LT
--   compare _ _     = EQ
