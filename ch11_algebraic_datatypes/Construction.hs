module Construction where

data GuessWhat =
  Chickenbutt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b =
  Product a b deriving (Eq, Show)

data Sum a b =
    First  a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst  :: a,
                  psecond :: b}
                  deriving (Eq, Show)

newtype NumCow =
  NumCow Int
  deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)

data Farmhouse =
  Farmhouse NumCow NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)

data BigFarmhouse =
  BigFarmhouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type BigFarmhouse' =
  Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo =
  CowInfo Name Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal =
    Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Animal' =
  Sum CowInfo (Sum PigInfo SheepInfo)


-- Constructing Values (pg. 649)

trivialValue :: GuessWhat
trivialValue = Chickenbutt

idInt :: Id Integer
idInt = MkId 10

type Awesome = Bool

person :: Product Name Awesome
person = Product "Simon" True

-- data Twitter =
--   Twitter deriving (Eq, Show)

-- data AskFm =
--   AskFm deriving (Eq, Show)

-- socialNetwork :: Sum Twitter AskFm
-- socialNetwork = First Twitter

-- data SocialNetwork =
--     Twitter
--   | AskFm
--   deriving (Eq, Show)

type Twitter = String
type AskFm = String

twitter :: Sum Twitter AskFm
twitter = First "Twitter"

-- Confusing to use type synonmyms
askfm :: Sum Twitter AskFm
askfm = First "AskFm"

-- myRecord :: RecordProduct Integer Float
-- myRecord = RecordProduct 42 0.00001

myRecord :: RecordProduct Integer Float
myRecord =
  RecordProduct { pfirst = 42,
                  psecond = 0.00001 }

data OperatingSystem =
    GnuPlusLinux
  | OpenBSD
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
    Haskell
  | Agda
  | Idris
  | Purescript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem,
               lang :: ProgLang }
  deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac, lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly =
  Programmer { lang = Agda, os = GnuPlusLinux }
