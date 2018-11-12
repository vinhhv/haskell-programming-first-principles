{-# LANGUAGE InstanceSigs #-}

module IdentityT where

import Control.Monad

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

--------------------------------------------------

newtype IdentityT f a = IdentityT { runIdentityT :: f a }

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance Applicative m => Applicative (IdentityT m) where
  pure a = IdentityT $ pure a
  IdentityT fab <*> IdentityT fa = IdentityT (fab <*> fa)

instance Monad m => Monad (IdentityT m) where
  return = pure

  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  IdentityT ma >>= f =
  --   let aimb = join $ fmap runIdentityT $ fmap f ma
  --   let aimb = join $ fmap (runIdentityT . f) ma
  --   let aimb = ma >>= runIdentityT . f
  --   in IdentityT aimb
    IdentityT $ ma >>= runIdentityT . f

