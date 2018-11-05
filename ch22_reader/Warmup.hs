module Warmup where

import Data.Char
import Control.Applicative

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = do
  a <- rev
  b <- cap
  return (a, b)

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> cap <*> rev

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = liftA2 (,) cap rev

tupledM :: [Char] -> ([Char], [Char])
tupledM = rev >>= (\r -> cap >>= (\c -> return (r, c)))
