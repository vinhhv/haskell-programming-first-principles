module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
              deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node t a t') = Node (fmap f t) (f a) (fmap f t')

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node t a t') = (foldMap f t) <> (f a) <> (foldMap f t')

  foldr _ z Empty = z
  foldr f z (Leaf a) = f a z
  foldr f z (Node t a t') = f a $ foldr f (foldr f z t) t'

instance Traversable Tree where
  traverse _ Empty = pure $ Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node t a t') = Node <$> traverse f t <*> f a <*> traverse f t'

genTree :: Arbitrary a => Gen (Tree a)
genTree = do
  a  <- arbitrary
  t  <- genTree
  t' <- genTree
  frequency [
      (2, return $ Node t a t'),
      (2, return $ Leaf a),
      (1, return Empty)
    ]

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genTree

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

main :: IO ()
main = do
  let triggerTree :: Tree (Int, Int, [Int])
      triggerTree = undefined
  quickBatch (functor triggerTree)
  quickBatch (traversable triggerTree)
