module Parametricity where

-- id function
onlyOneImplementation :: a -> a
onlyOneImplementation a = a


-- pair function
onlyTwoImplementations :: a -> a -> a
onlyTwoImplementations x y = onlyOneImplementation x
onlyTwoImplementations x y = onlyOneImplementation y

snd :: a -> b ->b
snd a b = b
