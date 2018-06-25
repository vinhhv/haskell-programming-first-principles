module Arith4 where

-- id :: a ->
-- id x = x

-- roundTrip :: (Show a, Read a) => a -> a
-- roundTrip a = read (show a)

-- 5)
roundTrip = read . show

-- 6)
roundTrip :: (Show a, Read b) => a -> b

main = do
  print (roundTrip 4 :: Int)
  print (id 4)
