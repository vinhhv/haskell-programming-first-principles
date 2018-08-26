module ForExample where

data Example = MakeExample deriving Show
data Example1 = MakeExample1 Int deriving Show

-- 1) :t MakeExample = MakeExample :: Example
-- 2) :info Example = typeclass instances include Show
-- 3) :t MakeExample1 = MakeExample1 :: Int -> Example1
