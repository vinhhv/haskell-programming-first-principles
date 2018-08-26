module Quad where

data Quad =
    One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

-- how many different forms can this take?

eQuad :: Either Quad Quad
eQuad = Right One
-- 4 + 4 = 8

prodQuad :: (Quad, Quad)
prodQuad = (One, One)
-- 4 * 4 = 16

funcQuad :: Quad -> Quad
funcQuad One   = One
funcQuad Two   = One
funcQuad Three = One
funcQuad Four  = One
-- 4 ^ 4 = 256

prodTBool :: (Bool, Bool, Bool)
prodTBool = (True, True, True)
-- 2 * 2 * 2 = 8

gTwo :: Bool -> Bool -> Bool
gTwo True True = True
gTwo True False = True
gTwo False True = True
gTwo False False = True
--    (2 ^ 2) ^ 2 = 16
-- OR 2 ^ (2 * 2) = 16

-- Hint: 5 digit number
fTwo :: Bool -> Quad -> Quad
fTwo True One    = One
fTwo True Two    = One
fTwo True Three  = One
fTwo True Four   = One
fTwo False One   = One
fTwo False Two   = One
fTwo False Three = One
fTwo False Four  = One
--    (4 ^ 4) ^ 2 = 65536
-- OR 4 ^ (4 * 2) = 65536
