module ValidateTheWord where

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter (`elem` vowels)

countConsonants :: String -> Integer
countConsonants = fromIntegral . length . filter (not . (`elem` vowels))

mkWord :: String -> Maybe Word'
mkWord [] = Nothing
mkWord xs = if v > c then Nothing else Just (Word' xs)
  where v = countVowels xs
        c = countConsonants xs
        -- c = (fromIntegral $ length xs) - countVowels xs
