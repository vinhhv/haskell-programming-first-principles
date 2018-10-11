module Combinations where

import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [(Char, Char, Char)]
combos = liftA3 (,,) stops vowels stops

combos' :: [(Char, Char, Char)]
combos' = (,,) <$> stops <*> vowels <*> stops
