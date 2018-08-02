module Factorial where

factorial :: Integer -> Integer
factorial 0 = 1
factorial i = i * factorial (i - 1)
