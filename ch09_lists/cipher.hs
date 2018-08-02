module Cipher where

import Data.Char

alphabetSize = 26

getLetterNum :: Char -> Int
getLetterNum c
  | isUpper c = ord c - ord 'A'
  | otherwise = ord c - ord 'a'

shiftLetter :: Int -> Int -> Int
shiftLetter shift letter = mod (letter + shift + alphabetSize) alphabetSize

getLetter :: Bool -> Int -> Char
getLetter isUp c
  | isUp      = chr . (+c) . ord $ 'A'
  | otherwise = chr . (+c) . ord $ 'a'

getStart :: Char -> Char
getStart c
  | isUpper c = 'A'
  | isLower c = 'a'
  | otherwise = c

cipherC :: Int -> Char -> Char
cipherC n c 
  | isLetter c = (getLetter (isUpper c)) . (shiftLetter n) . getLetterNum $ c
  | otherwise = c
