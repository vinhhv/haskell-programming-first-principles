module DoesItTypeCheck where

-- 1)
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn(show person)

-- 2)
data Mood = Blah
          | Woot deriving (Eq, Show)

settleDown x = if x == Woot
                 then Blah
                 else x

-- 3)
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- What can we do?
data Rocks =
  Rocks String deriving (Eq, Show, Ord)

data Yeah =
  Yeah Bool deriving (Eq, Show, Ord)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show, Ord)

-- phew = Papu "chases" True

truth = Papu (Rocks "chomskydoz")
             (Yeah True)

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
