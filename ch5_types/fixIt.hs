module FixIt where

-- module Sing where
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing x y = if (x > y) then fstString a else sndString b
  where a = "Singin"
        b = "Somewhere"
