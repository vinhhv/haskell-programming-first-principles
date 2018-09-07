module Cipher where

import Control.Monad
import Data.Char
import Test.QuickCheck

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
cipher s "" = s
cipher s e = cipherRecur s (foldr (++) "" (repeat e)) rightShift

decipher :: String -> String -> String
decipher s "" = s
decipher s e = cipherRecur s (foldr (++) "" (repeat e)) leftShift

cipherRecur :: String -> String -> (Int -> Char -> Char) -> String
cipherRecur [] _ _ = []
cipherRecur (' ': cs) e shift      = [' '] ++ cipherRecur cs e shift
cipherRecur (c : cs) (e: es) shift = [shift (ord e - ord 'A') c] ++ cipherRecur cs es shift

-- genCiphers :: Gen (String, String)
-- genCiphers = do
  -- OU a <- (arbitrary :: Gen OnlyUpper)
  -- OU b <- (arbitrary :: Gen OnlyUpper)
  -- return (a, b)

prop_cipherRoundTrip :: OnlyUpper -> OnlyUpper -> Bool
prop_cipherRoundTrip (OU msg) (OU key) =
  msg == ((flip decipher key) . (flip cipher key) $ msg)

newtype OnlyUpper = OU String deriving Show

instance Arbitrary OnlyUpper where
  arbitrary = fmap OU $ listOf $ choose ('A', 'Z')

main :: IO ()
main = do
  quickCheck prop_cipherRoundTrip
