module Main where

import Text.Parser.Combinators
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

oneEOF :: Parser ()
oneEOF = (char '1') >> eof

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

parse123 :: String -> IO ()
parse123 s = print $
  parseString (choice [string "123", string "12", string "1", stop])
  mempty s

string' :: String -> Parser String
string' str = go str mempty
  where go (x:xs) parsed = char x >>= (\y -> go xs $ parsed ++ [y])
        go [] parsed = return parsed

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "oneEOF:"
  print $ parseString oneEOF mempty "12"
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
