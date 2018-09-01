module EqCaseGuard where

data PersonInvalid = NameEmpty | AgeTooLow

-- Compiles without Eq
toString :: PersonInvalid -> String
toString NameEmpty = "NameEmpty"
toString AgeTooLow = "AgeTooLow"

instance Show PersonInvalid where
  show = toString

-- This does not work without an
-- Eq instance
-- blah :: PersonInvalid -> String
-- blah pi
--   | pi == NameEmpty = "NameEmpty"
--   | pi == AgeTooLow = "AgeTooLow"
--   | otherwise = "???"
