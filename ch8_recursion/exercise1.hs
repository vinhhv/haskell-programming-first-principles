module Exercise1 where

applyTimes :: (Eq a, Num a) =>
               a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

val = applyTimes 5 (+1) 5
-- (+1) . ((+1) . ((+1) . ((+1) . ((+1) $ 5 = 6) $ 5 = 7) $ 5 = 8) $ 5 = 9) $ 5 = 10
