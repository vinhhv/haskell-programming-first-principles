module Fibs where

fibs    = 0 : scanl (+) 1 fibs
fibsR   = 0 : scanr (+) 1 fibs
fibsN x = fibs !! x

fibs20  = take 20 fibs
fibs100 = takeWhile (<100) fibs

factorial n = take n $ scanl (*) 1 [2..]

