{-# LANGUAGE InstanceSigs #-}

module Exercises where

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

----------------------------------------

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

getDogRM' :: Reader Person Dog
getDogRM' = Reader $ Dog <$> dogName <*> address

----------------------------------------

ask :: Reader a a
ask = Reader id 

myLiftA2 :: Applicative f =>
            (a -> b -> c)
         -> f a -> f b -> f c
myLiftA2 f g c = f <$> g <*> c

asks :: (r -> a) -> Reader r a
asks f = Reader f

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus r = (foo r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

fooBind :: (r -> a)
        -> (a -> r -> b)
        -> (r -> b)
fooBind m k = \r -> k (m r) r
