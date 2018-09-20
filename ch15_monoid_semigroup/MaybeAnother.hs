module MaybeAnother where

import Data.Monoid (Monoid)
import Data.Semigroup
import Test.QuickCheck

asc :: Eq a => (a -> a -> a)
            -> a -> a -> a
            -> Bool
asc (<>) a b c =
  a <> (b <> c) == (a <> b) <> c

monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
  a `mappend` (b `mappend` c) == (a `mappend` b) `mappend` c

monoidLeftIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) (Only y) = Only (mappend x y)
  mappend (Only x) mempty   = Only x
  mappend mempty (Only y)   = Only y
  mappend Nada Nada         = mempty

genOnly :: Arbitrary a => Gen (Optional a)
genOnly = do
  a <- arbitrary
  return (Only a)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = genOnly

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' { getFirst' = Nada }
  mappend f@(First' { getFirst' = Only x }) _ = f
  mappend _ f@(First' { getFirst' = Only x }) = f
  mappend _ _                 = First' { getFirst' = Nada }

instance Semigroup (First' a) where
  (<>) f@(First' { getFirst' = Only x }) _ = f
  (<>) _ f@(First' { getFirst' = Only x }) = f
  (<>) _ _                 = First' { getFirst' = Nada }

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool

genFirst :: Arbitrary a => Gen (First' a)
genFirst = do
  a <- arbitrary
  return First' { getFirst' = a }

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = genFirst

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
