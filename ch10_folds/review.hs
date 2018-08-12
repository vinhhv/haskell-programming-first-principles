module Review where

stops  = "pbtdkg"
vowels = "aeiou"

e1a = [(x, y, z) | x <- stops, y <- vowels, z <- stops]
e1b = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

nouns = ["he", "she", "it", "poo"]
verbs = ["eats", "likes"]
e1c = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

-- 2a

seekritFunc x = div (sum (map length (words x))) (length (words x))
seekritFact x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))
