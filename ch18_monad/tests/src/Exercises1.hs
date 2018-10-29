module Main where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

---------------------------------------

data PhbtEither b a = Left' a | Right' b deriving (Eq, Show)

instance Functor (PhbtEither b) where
  fmap f (Left' a)  = Left' (f a)
  fmap _ (Right' b) = Right' b

instance Applicative (PhbtEither b) where
  pure = Left'
  Left' f  <*> l = fmap f l
  Right' b <*> _ = Right' b

instance Monad (PhbtEither b) where
  return = pure
  Left' a >>= f = f a
  Right' b >>= _ = Right' b

genPhbt :: (Arbitrary b, Arbitrary a) => Gen (PhbtEither b a)
genPhbt = do
  b <- arbitrary
  a <- arbitrary
  elements [(Left' a), (Right' b)]

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhbtEither b a) where
  arbitrary = genPhbt

instance (Eq b, Eq a) => EqProp (PhbtEither b a) where
  (=-=) = eq

----------------------------------------

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genIdentity

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

----------------------------------------

data List a = Nil | Cons a (List a) deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil xs = xs
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  Cons f fs <*> xs = append (fmap f xs) (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x xs >>= f = append (f x) (xs >>= f)

genList :: Arbitrary a => Gen (List a)
genList = do
  a <- arbitrary
  l <- arbitrary
  frequency[(1, return Nil), (2, return (Cons a l))]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

instance Eq a => EqProp (List a) where
  (=-=) = eq

type TypeT = (Int, Int, Int)

main = do
  let triggerNope :: Nope TypeT
      triggerNope = undefined
  let triggerPhbt :: PhbtEither TypeT TypeT
      triggerPhbt = undefined
  let triggerIden :: Identity TypeT
      triggerIden = undefined
  let triggerList :: List TypeT
      triggerList = undefined
  quickBatch $ functor triggerNope
  quickBatch $ applicative triggerNope
  quickBatch $ monad triggerNope
  quickBatch $ functor triggerPhbt
  quickBatch $ applicative triggerPhbt
  quickBatch $ monad triggerPhbt
  quickBatch $ functor triggerIden
  quickBatch $ applicative triggerIden
  quickBatch $ monad triggerIden
  quickBatch $ functor triggerList
  quickBatch $ applicative triggerList
  quickBatch $ monad triggerList
