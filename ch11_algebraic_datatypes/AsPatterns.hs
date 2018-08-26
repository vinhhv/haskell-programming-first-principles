module AsPatterns where

import Data.Char

isSubseqOf :: (Eq a)
           => [a]
           -> [a]
           -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xss@(x: xs) (y: ys) =
  if x == y
  then isSubseqOf xs ys
  else isSubseqOf xss ys

-- (x == y && isSubseqOf xs ys) || isSubseqOf xss ys // not efficient, runs fully if fails

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map (capitalize) . words $ s

capitalize :: String -> (String, String)
capitalize word@(x: xs) = (word, toUpper x : xs)

main :: IO ()
main = do
  print "isSubseqOf"
  print (isSubseqOf "blah" "blahwoot")
  print (isSubseqOf "blah" "wootblah")
  print (isSubseqOf "blah" "wboloath")
  print (isSubseqOf "blah" "wootbla")
  print (isSubseqOf "blah" "halbwoot")
  print (isSubseqOf "blah" "blawhoot")
  print '\n'
  
  print "capitalizeWords"
  print (capitalizeWords "hello there")
