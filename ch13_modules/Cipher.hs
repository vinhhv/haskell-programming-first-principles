module Cipher where

import Control.Monad (forever)
import Data.Char

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

decipher :: String -> String -> String
decipher s e = cipherRecur s (foldr (++) "" (repeat e)) leftShift

cipherRecur :: String -> String -> (Int -> Char -> Char) -> String
cipherRecur [] _ _ = []
cipherRecur (' ': cs) e shift      = [' '] ++ cipherRecur cs e shift
cipherRecur (c : cs) (e: es) shift = [shift (ord e - ord 'A') c] ++ cipherRecur cs es shift

executeCmd :: [String] -> IO ()
executeCmd command =
  if length command /= 3
  then putStrLn "Only provide 3 commands"
  else case command of
      ("cipher": (s: (code: []))) -> putStrLn $ cipher s code
      ("decipher": (s: (code: []))) -> putStrLn $ decipher s code
      _ -> putStrLn "Invalid commands provided"

main :: IO ()
main = forever $ do
  putStr "Enter a command '[cipher|decipher] STRING CODE': "
  input <- getLine 
  let command = words input
  executeCmd command
