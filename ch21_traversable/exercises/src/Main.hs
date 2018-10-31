module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldr f z (Identity a) = f a z
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genIdentity

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

----------------------------------------

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a

genConstant :: Arbitrary a => Gen (Constant a b)
genConstant = do
  a <- arbitrary
  return (Constant { getConstant = a })

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = genConstant

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

----------------------------------------

data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

genTraversable :: Arbitrary a => Gen (Optional a)
genTraversable = do
  a <- arbitrary
  elements [Nada, Yep a]

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = genTraversable

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

----------------------------------------

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Foldable List where
  foldMap _ Nil = mempty 
  foldMap f (Cons x xs) = (f x) <> (foldMap f xs)

instance Traversable List where
  traverse f Nil = pure Nil
  traverse f (Cons x xs)  = Cons <$> f x <*> traverse f xs

genList :: Arbitrary a => Gen (List a)
genList = do
  a <- arbitrary
  l <- genList
  frequency [(3, return $ Cons a l), (1, return Nil)]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

instance Eq a => EqProp (List a) where
  (=-=) = eq

----------------------------------------

data Three a b c = Three a b c deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
genThree = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = genThree

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

----------------------------------------

data Pair a b = Pair a b deriving (Eq, Ord, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

genPair :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
genPair = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = genPair

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq



main :: IO ()
main = do
  let triggerI :: Identity (Int, Int, [Int])
      triggerI = undefined
  let triggerC :: Constant (Int, Int, [Int]) (Int, Int, [Int])
      triggerC = undefined
  let triggerO :: Optional (Int, Int, [Int])
      triggerO = undefined
  let triggerL :: List (Int, Int, [Int])
      triggerL = undefined
  let triggerT :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int])
      triggerT = undefined
  let triggerP :: Pair (Int, Int, [Int]) (Int, Int, [Int])
      triggerP = undefined
  quickBatch (functor triggerI)
  quickBatch (traversable triggerI)
  quickBatch (functor triggerC)
  quickBatch (traversable triggerC)
  quickBatch (functor triggerO)
  quickBatch (traversable triggerO)
  quickBatch (functor triggerL)
  quickBatch (traversable triggerL)
  quickBatch (functor triggerT)
  quickBatch (traversable triggerT)
  quickBatch (functor triggerP)
  quickBatch (traversable triggerP)
