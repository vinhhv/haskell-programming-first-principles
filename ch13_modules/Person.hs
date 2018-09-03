module Person where

import Control.Monad (forever)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
    Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++
      " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = forever $ do
  putStrLn "Creating a person..."
  putStr "Name: "
  name <- getLine
  putStr "Age: "
  ageStr <- getLine
  let age = read ageStr :: Integer
  let person = mkPerson name age
  case person of
    Right person -> putStrLn $ "Person is " ++ show person
    Left error -> putStrLn $ "Error: " ++ show error
