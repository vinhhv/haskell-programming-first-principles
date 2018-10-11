module Constant where

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant f) = Constant f

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant f) (Constant f') = Constant (mappend f f')
