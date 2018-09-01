module CountTheBeforeVowel where

isVowel :: Char -> Bool
isVowel c = any (==c) ['a', 'e', 'i', 'o', 'u']

theBeforeVowel :: String -> String -> Integer
theBeforeVowel "the" word = if isVowel (head word) then 1 else 0
theBeforeVowel _ []       = 0
theBeforeVowel _ _        = 0

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel xs = go (words xs)
  where go [] = 0
        go (x: []) = 0
        go (x: xs) = theBeforeVowel x (head xs) + go xs
