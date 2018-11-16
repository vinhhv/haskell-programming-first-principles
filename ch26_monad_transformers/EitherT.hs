{-# LANGUAGE InstanceSigs #-}

module EitherT where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ (pure . pure) x
  EitherT fmab <*> EitherT mea = EitherT $ (<*>) <$> fmab <*> mea

instance Monad m => Monad (EitherT e m) where
  return = pure
  EitherT mea >>= f =
    EitherT $ do
      e <- mea
      case e of
        Left l -> return $ Left l 
        Right r -> runEitherT (f r)

swapEither :: Either e a -> Either a e
swapEither (Right r) = Left r
swapEither (Left l) = Right l

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea 

eitherT :: Monad m => (a -> m c) -> (b -> m c)
                   -> EitherT a m b -> m c
eitherT f g (EitherT mab) = do
  e <- mab
  case e of
    Left l -> f l
    Right r -> g r
