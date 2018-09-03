module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (sort)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- type WordList = [String]
newtype WordList = WordList [String] deriving (Eq, Show)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in     l >= minWordLength
              && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, (length wl - 1))
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
  Puzzle String [Maybe Char] [Char] Integer

instance Show Puzzle where
  show (Puzzle _ discovered guessed wrong) =
    (intersperse ' ' $
     fmap renderPuzzleChar discovered)
    ++ "\nGuessed so far: " ++ guessed
    ++ "\nWrong guesses: " ++ show wrong

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word discovered ([] :: String) 0
  where discovered = map (\w -> Nothing) word

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _ _) = (`elem` word)

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) = (`elem` guessed)

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Integer -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s cnt) c wrong =
  Puzzle word newFilledInSoFar (sort (c : s)) (cnt + wrong)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "Correct guess! Filling in..."
      return (fillInCharacter puzzle guess 0)
    (False, _) -> do
      putStrLn "Wrong guess, you stupid idiot!"
      return (fillInCharacter puzzle guess 1)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ wrong) =
  if wrong >= 9 then
    do putStrLn "You lose, you suck!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle wordToGuess filledInSoFar _ _) =
  if all isJust filledInSoFar then
    do putStrLn "You win, you don't suck!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character, idiot!"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
