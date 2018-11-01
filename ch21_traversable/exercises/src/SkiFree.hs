{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S n a) = foldMap f n <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a

genS :: (Arbitrary (n a), CoArbitrary (n a),
         Arbitrary a, CoArbitrary a) => Gen (S n a)
genS = do
  n <- arbitrary
  a <- arbitrary
  return $ S (n a) a

instance (Arbitrary (n a), CoArbitrary (n a),
         Arbitrary a, CoArbitrary a) => Arbitrary (S n a) where
  arbitrary = genS


instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq

main = do
  let trigger :: S [] (Int, Int, [Int])
      trigger = undefined
  sample' (arbitrary :: Gen (S [] Int))
  quickBatch (functor trigger)
  quickBatch (traversable trigger)
