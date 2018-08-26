module Exercise where

-- 1)
data Weekday =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

-- a) Weekday is a type with five data constructors

f Friday = "Miller Time"
-- 2) :t f = f :: Weekday -> String

-- 3) Types defined with the data keyword b) must begin with a capital letter

-- 4) 
g xs = xs !! (length xs - 1)
