module MyWords where

myWords :: String -> [String]
myWords []      = []
myWords (' ':s) = myWords s
myWords s       = w : myWords t
  where
    w = takeWhile (/= ' ') s
    t = dropWhile (/= ' ') s

-- myWords string = snd $ collectWords string []
--   where collectWords x xs
--          | length x == 0 = (x, xs)
--          | otherwise = collectWords remainder words
--            where remainder = dropWhile (== ' ') . dropWhile (/= ' ') $ x
--                  words = xs ++ [(takeWhile (/= ' ') x)]
