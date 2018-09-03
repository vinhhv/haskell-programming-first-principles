module Palindrome where

import Control.Monad
import Data.Char
import System.Exit (exitSuccess)

isPalindrome :: String -> Bool
isPalindrome line = parsed == reverse parsed
  where parsed = map toLower . filter isLetter $ line

palindrome :: IO ()
palindrome = forever $ do
  line <- getLine
  case isPalindrome line of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Wrong-oh! Good-bye!"
      exitSuccess

