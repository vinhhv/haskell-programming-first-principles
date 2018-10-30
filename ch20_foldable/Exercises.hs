module Exercises where

import Data.Monoid

data Constant a b = Constant b

instance Foldable (Constant a) where
  foldr f z (Constant b) = f b z
  foldMap f (Constant b) = f b


data Two a b = Two a b

instance Foldable (Two a) where
  foldr f z (Two _ b) = f b z
  foldMap f (Two _ b) = f b


data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z
  foldMap f (Three _ _ c) = f c


data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = (f b) <> (f b')


data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = (f b) <> (f b') <> (f b'')


filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
