-- FunctionWithWhere.hs
module FunctionWithWhere where

printInc n = print plusTwo
  where plusTwo = n + 2

printInc2 n = let plusTwo = n + 2
              in print plusTwo

mult1     = x * y
  where x = 5
        y = 6

-- Rewrite in where clauses
-- let x = 3; y = 1000 in x * 3 + y
mult2     = x * 3 + y
  where y = 1000
        x = 3

-- let y = 10; x = 10 * 5 + y in x * 5
mult3     = x * 5
  where y = 10
        x = 10 * 5 + y

-- let x = 7; y = negate x; z = y * 10 in z / x + y
mult4     = z / x + y
  where x = 7
        y = negate x
        z = y * 10

waxOn     = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7

triple x = x * 3

waxOff x = triple x
