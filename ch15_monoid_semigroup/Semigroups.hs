module Semigroups where

import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

----------------------------------

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genIdentity

type IdentityAssoc =
  Identity String -> Identity String -> Identity String -> Bool

----------------------------------

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo

type TwoAssoc =
  Two String (Sum Int) -> Two String (Sum Int) -> Two String (Sum Int) -> Bool

----------------------------------

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
genThree = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = genThree

type ThreeAssoc = Three String (Sum Int) (Product Int)
               -> Three String (Sum Int) (Product Int)
               -> Three String (Sum Int) (Product Int)
               -> Bool

----------------------------------

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

genFour :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
genFour = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = genFour

type FourAssoc = Four String (Sum Int) (Product Int) ([Product Int])
              -> Four String (Sum Int) (Product Int) ([Product Int])
              -> Four String (Sum Int) (Product Int) ([Product Int])
              -> Bool

----------------------------------

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj b) <> (BoolConj b') = BoolConj (b && b')

genBoolConj :: Gen BoolConj
genBoolConj = do
  a <- arbitrary
  return (BoolConj a)
  
instance Arbitrary BoolConj where
  arbitrary = genBoolConj

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

----------------------------------

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj b) <> (BoolDisj b') = BoolDisj (b || b')

genBoolDisj :: Gen BoolDisj
genBoolDisj = do
  a <- arbitrary
  return (BoolDisj a)
  
instance Arbitrary BoolDisj where
  arbitrary = genBoolDisj

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

----------------------------------

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst a) <> (Fst a') = (Fst a')
  (Fst a) <> (Snd b)  = (Snd b)
  (Snd b) <> (Fst a)  = (Snd b)
  (Snd b) <> (Snd b') = (Snd b)

genOr :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
genOr = do
  a <- arbitrary
  b <- arbitrary
  frequency [(1, return (Fst a)), (1, return (Snd b))]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = genOr

type OrAssoc = Or String Int
            -> Or String Int
            -> Or String Int
            -> Bool

----------------------------------

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

----------------------------------

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  (Comp a) <> (Comp a') = Comp (a <> a')

----------------------------------

data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Success' b) (Success' b') = Success' b
  (<>) (Failure' a) (Success' b ) = Success' b
  (<>) (Success' b) (Failure' a ) = Success' b
  (<>) (Failure' a) (Failure' a') = Failure' (a <> a')

genValidation :: (Arbitrary a, Arbitrary b) => Gen (Validation a b)
genValidation = do
  a <- arbitrary
  b <- arbitrary
  elements [(Failure' a), (Success' b)]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = genValidation

type ValidationAssoc = Validation String Int
                    -> Validation String Int
                    -> Validation String Int
                    -> Bool

----------------------------------

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c =
  a <> (b <> c) == (a <> b) <> c

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (semigroupAssoc :: ValidationAssoc)
