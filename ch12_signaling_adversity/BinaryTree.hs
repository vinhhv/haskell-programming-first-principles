module BinaryTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

addUp :: Integer -> Integer -> Maybe (Integer, Integer, Integer)
addUp x limit
  | x < 0  = Nothing
  | x < limit = Just (x + 1, x, x + 1)
  | x > limit  = Nothing

unfold :: (a -> Maybe (a, b, a))
       -> a
       -> BinaryTree b
unfold f a =
  case f a of
    Just (x, y, z) -> Node (unfold f x) y (unfold f z)
    Nothing        -> Leaf

treeBuild' :: Integer -> Integer -> Maybe (Integer, Integer, Integer)
treeBuild' limit x
  | x < 0      = Nothing
  | x < limit  = Just (x + 1, x, x + 1)
  | x >= limit = Nothing

treeBuild :: Integer -> BinaryTree Integer
treeBuild limit = unfold (treeBuild' limit) 0
