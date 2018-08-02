module Filtering where

f1 = filter (\x -> rem x 3 == 0) $ [1..30]
f2 = length . filter (\x -> rem x 3 == 0) $ [1..30]

notArticle :: String -> Bool
notArticle "the" = False
notArticle "a"   = False
notArticle "an"  = False
notArticle _     = True

myFilter :: String -> [String]
myFilter x = filter notArticle . words $ x
