module Cipher where

import Data.Char

-- changeLetter :: Char -> Int -> Char
-- changeLetter c i
--   | 'A' <= c && c <= 'Z' = chr ((ord c - ord 'A' + i) `mod` 26 + ord 'A')
--   | otherwise = c

encode :: Char -> Int
encode c = ord c - ord 'A'

decode :: Int -> Char
decode n = chr (ord 'A' + n)

shift :: (Int -> Int -> Int) -> Int -> Char -> Char
shift f n c = decode $ mod (f (encode c) n) 26

rightShift :: Int -> Char -> Char
rightShift = shift (+)

leftShift :: Int -> Char -> Char
leftShift = shift (-)

cipher :: String -> String -> String
cipher s e = cipherRecur s (foldr (++) "" (repeat e)) rightShift

uncipher :: String -> String -> String
uncipher s e = cipherRecur s (foldr (++) "" (repeat e)) leftShift

cipherRecur :: String -> String -> (Int -> Char -> Char) -> String
cipherRecur [] _ _ = []
cipherRecur (' ': cs) e shift      = [' '] ++ cipherRecur cs e shift
cipherRecur (c : cs) (e: es) shift = [shift (ord e - ord 'A') c] ++ cipherRecur cs es shift
