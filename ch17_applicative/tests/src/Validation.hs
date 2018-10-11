module Main where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a =
    Failure' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Success' a) = Success' (f a)
  fmap _ (Failure' e) = Failure' e

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (<*>) (Success' f) (Success' a)  = Success' (f a)
  (<*>) (Failure' e) (Failure' e') = Failure' (e <> e')
  (<*>) (Failure' e) _            = Failure' e
  (<*>) _ (Failure' e)            = Failure' e

genValidation :: (Arbitrary e, Arbitrary a) => Gen (Validation e a)
genValidation = do
  e <- arbitrary
  a <- arbitrary
  frequency [(2, return (Success' a)), (1, return (Failure' e))]

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = genValidation

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

test :: Validation String (String, String, String)
test = undefined

main :: IO ()
main = do
  quickBatch $ applicative test
