module MyParse where

myParse :: String -> Char -> [String]
myParse [] _      = []
myParse (c:cs) ch = if ch == c then myParse cs ch else h : myParse t ch
  where
    h = takeWhile (/= ch) (c : cs)
    t = dropWhile (/= ch) (c : cs)

myLines :: String -> [String]
myLines s = myParse s '\n'

myWords :: String -> [String]
myWords s = myParse s ' '
