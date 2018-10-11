module Main where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- BadMonoid

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools),
                         (1, return Twoo)]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools

instance EqProp Bull where (=-=) = eq

-- List Applicative

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) xs = append (fmap f xs) (fs <*> xs)

genList :: Arbitrary a => Gen (List a)
genList = do
  a <- arbitrary
  l <- genList
  frequency [(1, return Nil), (2, return (Cons a l))]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b)
        -> List a
        -> List b
flatMap f as = concat' (fmap f as)

instance Eq a => EqProp (List a) where
  (=-=) xs ys = xs' `eq` ys'
        where xs' = let l = xs
                    in take' 3000 l
              ys' = let l = ys
                    in take' 3000 l

take' :: Int -> List a -> List a
take' 0 _   = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

zipLists :: List (a -> b) -> List a -> List b
zipLists Nil _ = Nil
zipLists _ Nil = Nil
zipLists (Cons f fs) (Cons a Nil) = Cons (f a) (fs <*> pure a)
zipLists (Cons f Nil) (Cons a as) = Cons (f a) (pure f <*> as)
zipLists (Cons f fs) (Cons a as)  = Cons (f a) (zipLists fs as)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList'(Cons x Nil)
  (<*>) (ZipList' fs) (ZipList' xs) = ZipList' (zipLists fs xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  (=-=) xs ys = xs' `eq` ys'
        where xs' = let (ZipList' l) = xs
                    in take' 3000 l
              ys' = let (ZipList' l) = ys
                    in take' 3000 l

main :: IO ()
main = do
  quickBatch (monoid Twoo)
  quickBatch $ applicative (Cons ("a", "b", "c") Nil)
  quickBatch $ applicative (ZipList' (Cons ("a", "b", "c") Nil))
