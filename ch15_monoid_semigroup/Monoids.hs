module Monoids where

import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty  = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

----------------------------------

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Monoid a, Semigroup a) => Monoid (Identity a) where
  mempty  = Identity (mempty)
  mappend = (<>)

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

instance (Monoid a, Monoid b, Semigroup a, Semigroup b) => Monoid (Two a b) where
  mempty  = Two mempty mempty
  mappend = (<>)

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

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj b) <> (BoolConj b') = BoolConj (b && b')

instance Monoid BoolConj where
  mempty  = BoolConj True
  mappend = (<>)

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

instance Monoid BoolDisj where
  mempty  = BoolDisj False
  mappend = (<>)

genBoolDisj :: Gen BoolDisj
genBoolDisj = do
  a <- arbitrary
  return (BoolDisj a)
  
instance Arbitrary BoolDisj where
  arbitrary = genBoolDisj

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

----------------------------------

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
  mempty  = Combine mempty
  mappend = (<>)

----------------------------------

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  (Comp a) <> (Comp a') = Comp (a <> a')

instance (Monoid a, Semigroup a) => Monoid (Comp a) where
  mempty  = Comp mempty
  mappend = (<>)

----------------------------------

newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
  }

instance Semigroup a => Semigroup (Mem s a) where
  (<>) (Mem { runMem = f }) (Mem { runMem = g }) = Mem $ \s ->
    let (a,  b) = g s
        (a', c) = f b
     in 
        (a <> a', c)

instance (Monoid a, Semigroup a) => Monoid (Mem s a) where
  mempty  = Mem $ \s -> (mempty, s)
  mappend = (<>)

f' = Mem $ \s -> ("hi", s + 1)

----------------------------------

sa :: (Eq m, Semigroup m) => m -> m -> m -> Bool
sa a b c = a <> (b <> c) == (a <> b) <> c

mli :: (Eq m, Monoid m) => m -> Bool
mli a = (mempty `mappend` a) == a

mri :: (Eq m, Monoid m) => m -> Bool
mri a = (a `mappend` mempty) == a

main :: IO ()
main = do
  quickCheck (sa  :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mri :: Trivial -> Bool)
  quickCheck (sa  :: IdentityAssoc)
  quickCheck (mli :: Identity String -> Bool)
  quickCheck (mri :: Identity String -> Bool)
  quickCheck (sa  :: TwoAssoc)
  quickCheck (mli :: Two String (Sum Int) -> Bool)
  quickCheck (mri :: Two String (Sum Int) -> Bool)
  quickCheck (sa  :: BoolConjAssoc)
  quickCheck (mli :: BoolConj -> Bool)
  quickCheck (mri :: BoolConj -> Bool)
  quickCheck (sa  :: BoolDisjAssoc)
  quickCheck (mli :: BoolDisj -> Bool)
  quickCheck (mri :: BoolDisj -> Bool)

  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ ((runMem mempty 0) :: (String, Int))
  print $ (runMem (f' <> mempty) 0) == runMem f' 0
  print $ (runMem (mempty <> f') 0) == runMem f' 0
