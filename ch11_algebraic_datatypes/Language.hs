module Language where

import Data.Char
import Data.List

capitalizeWord :: String -> String
capitalizeWord (x: xs) = toUpper x : xs

capitalize :: [String] -> [String]
capitalize []    = []
capitalize words = capitalizeRecur words True

capitalizeRecur :: [String] -> Bool -> [String]
capitalizeRecur [] _         = []
capitalizeRecur (x: xs) True  = capitalizeWord x : capitalizeRecur xs (shouldCapitalize x)
capitalizeRecur (x: xs) False = x : capitalizeRecur xs (shouldCapitalize x)

shouldCapitalize :: String -> Bool
shouldCapitalize [] = False
shouldCapitalize x = x !! (length x - 1) == '.'

capitalizeParagraph :: String -> String
capitalizeParagraph = intercalate " " . capitalize . words

-- Cleaner solution without being forced to use capitalizeWord
capitalizeParagraph' :: String -> String
capitalizeParagraph' = capitalize' True
  where
    capitalize' _ []          = ""
    capitalize' _ ('.': xs)   = '.' : capitalize' True xs
    capitalize' b (' ': xs)   = ' ' : capitalize' b xs
    capitalize' True (x: xs)  = toUpper x : capitalize' False xs
    capitalize' False (x: xs) = x : capitalize' False xs
