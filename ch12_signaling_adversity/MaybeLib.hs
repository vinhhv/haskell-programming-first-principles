module MaybeLib where

isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
-- fromMaybe a Nothing  = a
-- fromMaybe a (Just b) = b
fromMaybe b a = mayybee b id a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just $ head xs

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes []             = []
catMaybes (Nothing: xs)  = catMaybes xs
catMaybes ((Just a): xs) = a : catMaybes xs

flipMaybe :: Eq a => [Maybe a] -> Maybe [a]
flipMaybe m
  | elem Nothing m = Nothing
  | otherwise      = Just $ go m
  where go ((Just x): xs) = x : go xs
        go [] = []
