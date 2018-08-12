module DB where

import Data.List
import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [
    DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, world!",
    DbNumber 9002,
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

extractTime :: DatabaseItem -> [UTCTime] -> [UTCTime]
extractTime (DbDate date) times = [date] ++ times
extractTime _ times             = times

extractTimeL :: [UTCTime] -> DatabaseItem -> [UTCTime]
extractTimeL times (DbDate date) = times ++ [date]
extractTimeL times _             = times

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr extractTime []

filterDbDateL :: [DatabaseItem] -> [UTCTime]
filterDbDateL = foldl extractTimeL []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (\a b -> case a of
                                  (DbNumber n) -> [n] ++ b
                                  _            -> b) []

filterDbNumberL :: [DatabaseItem] -> [Integer]
filterDbNumberL = foldl (\b a -> case a of
                                  (DbNumber n) -> b ++ [n]
                                  _            -> b) []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avg :: [Integer] -> Double
avg xs = fromInteger (sum xs) / genericLength xs

avgDb :: [DatabaseItem] -> Double
avgDb = avg . filterDbNumber
