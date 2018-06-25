module WriteFunc where

i :: a -> a
i = id

c :: a -> b -> a
c x y = x

c'' :: b -> a -> b
c'' x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r x = x
-- r x = tail x
-- r x = take 5 x
-- r x = drop 5 x

co :: (b -> c) -> (a -> b) -> a -> c
co g f x = g (f x)

a :: (a -> c) -> a -> a
a f x = x

a' :: (a -> b) -> a -> b
a' f x = f x
