module StateT where

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s ->
    fmap (\(a, s1) -> (f a, s1)) $ sma s

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  StateT smab <*> StateT sma = StateT $ \s -> do
  (ab, s1) <- smab s
  (a, s2) <- sma s1
  return (ab a, s2)

instance Monad m => Monad (StateT s m) where
  return = pure
  StateT sma >>= f =
    StateT $ \s -> do
      (a, s1) <- sma s
      runStateT (f a) s1
