module Exercises where

import Data.Char

getUpper :: String -> String
getUpper s = filter isUpper s

capitalize :: String -> String
capitalize []      = []
capitalize (x: xs) = toUpper x : xs

toUpperAll :: String -> String
toUpperAll []      = []
toUpperAll (x: xs) = toUpper x : toUpperAll xs

getFirstLetter :: String -> Maybe Char
getFirstLetter [] = Nothing
getFirstLetter xs = Just . toUpper . head $ xs

getFirstLetterPF :: String -> Maybe Char
getFirstLetterPF = Just . toUpper . head
