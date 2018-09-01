module ReplaceThe where

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

appendWords :: String -> Maybe String -> String
appendWords [] Nothing        = "a"
appendWords [] (Just s)       = s
appendWords sentence Nothing  = sentence ++ " " ++ "a"
appendWords sentence (Just s) = sentence ++ " " ++ s

replaceThe :: String -> String
replaceThe = foldl appendWords "" . map notThe . words
