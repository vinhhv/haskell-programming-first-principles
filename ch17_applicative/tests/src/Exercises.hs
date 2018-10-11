module Main where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1) []

pureList a = pure a :: [Int]
applyList a b = pure a <*> pure b :: [Int]

-- 2) IO

pureIO a = pure a :: IO String
applyIO a b = pure a <*> pure b :: IO String

-- 3) (,) a

pureTuple a = pure a :: (String, Int)
applyTuple a b = pure a <*> pure b :: (String, Int)

-- 4) (->) e

-- x = pureFunction 1
-- x "hello" == 1
pureFunction a = pure a :: (String -> Int)

-- applyFunction (\x -> 2) "hello" == ("", 2)
applyFunction a b = pure a <*> pure b :: (String, Int)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f f') (Pair a a') = Pair (f a) (f a')

genPair :: Arbitrary a => Gen (Pair a)
genPair = do
  a  <- arbitrary
  return $ Pair a a

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = genPair

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  (<*>) (Two a f) (Two a' b) = Two (a <> a') (f b)

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
  a <- arbitrary
  b <- arbitrary
  return $ Two a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  (<*>) (Three a b f) (Three a' b' c) = Three (a <> a') (b <> b') (f c)

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
genThree = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return $ Three a b c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = genThree

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' a f f') (Three' a' b b') = Three' (a <> a') (f b) (f' b')

genThree' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
genThree' = do
  a <- arbitrary
  b <- arbitrary
  return $ Three' a b b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = genThree'

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  (<*>) (Four a b c f) (Four a' b' c' d) = Four (a <> a') (b <> b') (c <> c') (f d)

genFour :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
genFour = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four a b c d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = genFour

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Monoid a => Applicative (Four' a) where
  pure b = Four' mempty mempty mempty b
  (<*>) (Four' a b c f) (Four' a' b' c' d) = Four' (a <> a') (b <> b') (c <> c') (f d)

genFour' :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
genFour' = do
  a <- arbitrary
  b <- arbitrary
  return $ Four' a a a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = genFour'

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

t = ("a", "b", "c")

main :: IO ()
main = do
  quickBatch $ applicative (Pair t t) 
  quickBatch $ applicative (Two t t) 
  quickBatch $ applicative (Three t t t)
  quickBatch $ applicative (Three' t t t)
  quickBatch $ applicative (Four t t t t)
  quickBatch $ applicative (Four' t t t t)
