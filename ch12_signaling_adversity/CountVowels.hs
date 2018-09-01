module CountVowels where

isVowel :: Char -> Bool
isVowel = (`elem` "aeiou")

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel
