module Exercises where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap f (Moi g) =
    Moi (\s -> let (a, s1) = g s
               in (f a, s1))

instance Applicative (Moi s) where
  pure a = Moi (\s -> (a, s))

  (Moi f) <*> (Moi g) = Moi (
    \s -> let (fab, s1) = f s
              (a,   s2) = g s1
          in (fab a, s2))

instance Monad (Moi s) where
  return = pure

  (Moi f) >>= g = Moi (
    \s -> let (a, s1) = f s
              m       = g a
          in runMoi m s1)

----------------------------------------

get :: Moi s s
get = Moi (\s -> (s, s))

put :: s -> Moi s ()
put st = Moi (\s -> ((), st))

exec :: Moi s a -> s -> s
exec m s = snd $ runMoi m s

eval :: Moi s a -> s -> a
eval m s = fst $ runMoi m s

modify :: (s -> s) -> Moi s ()
modify f = Moi (\s -> ((), f s))
