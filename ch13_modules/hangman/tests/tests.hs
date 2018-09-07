module Main where

import Hangman
import Test.Hspec

main :: IO ()
main = hspec $ do
  let puzzle = freshPuzzle "abc"
  describe "fillInCharacter" $ do
    it "returns the same puzzle when given an wrong guess" $ do
      fillInCharacter puzzle 'z' 1
      `shouldBe`
      Puzzle "abc" [Nothing, Nothing, Nothing] ['z'] 1
    it "returns a different puzzle when given a correct guess" $ do
      fillInCharacter puzzle 'a' 0
      `shouldBe`
      Puzzle "abc" [Just 'a', Nothing, Nothing] ['a'] 0

  describe "handleGuess" $ do
    it "returns an incorrect response for wrong guesses" $ do
      handleGuess puzzle 'z'
      `shouldReturn`
      Puzzle "abc" [Nothing, Nothing, Nothing] ['z'] 1
