module EitherLib where

lefts' :: [Either a b] -> [a]
lefts' = foldr left []
  where left (Left a) b  = a : b
        left (Right _) b = b

rights' :: [Either a b] -> [b]
rights' = foldr right []
  where right (Left _) b  = b
        right (Right a) b = a : b

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' f _ (Left a)  = f a
either' _ f (Right b) = f b

eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
