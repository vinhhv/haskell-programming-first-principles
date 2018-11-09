module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo from to =
  if from == to + 1
  then []
  else fizzBuzz from : fizzBuzzFromTo (from + 1) to

main :: IO ()
main = do
  mapM_ putStrLn $ reverse $ fizzBuzzList [1..10]
  putStrLn $ show $ fizzBuzzFromTo 10 20
