module Optional where

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) (Only y) = Only (mappend x y)
  mappend (Only x) mempty   = Only x
  mappend mempty (Only y)   = Only y
  mappend Nada Nada         = mempty
