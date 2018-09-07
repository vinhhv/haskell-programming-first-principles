module QC where

import Data.Char
import Data.List
import Test.Hspec
import Test.QuickCheck

half x = x / 2

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x,  t) = (Just y, x >= y)

plusAssociate x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x

prop_isAssociative :: Int -> Int -> Int -> Bool
prop_isAssociative = plusAssociate

prop_isCommutative :: Int -> Int -> Bool
prop_isCommutative = plusCommutative

multiAssociate x y z = x * (y * z) == (x * y) * z
multiCommutative x y = x * y == y * x

prop_isAssociativeM :: Int -> Int -> Int -> Bool
prop_isAssociativeM = multiAssociate

prop_isCommutativeM :: Int -> Int -> Bool
prop_isCommutativeM = multiCommutative

powerAssociate x y z = x ^ (y ^ z) == (x ^ y) ^ z
powerCommutative x y = x ^ y == y ^ x

prop_isAssociativeP :: Int -> Int -> Int -> Bool
prop_isAssociativeP = powerAssociate

prop_isCommutativeP :: Int -> Int -> Bool
prop_isCommutativeP = powerCommutative

----------------------------------------

quotRemT x (NonZero y) = (quot x y)*y + (rem x y) == x
divModT x (NonZero y)  = (div x y)*y + (mod x y) == x

prop_quotRem :: Int -> NonZero Int -> Bool
prop_quotRem = quotRemT

prop_divModT :: Int -> NonZero Int -> Bool
prop_divModT = divModT

----------------------------------------

prop_reverseTwiceIsId :: Ord a => [a] -> Bool
prop_reverseTwiceIsId l = (reverse . reverse $ l) == id l 

----------------------------------------

f x = x + 10
g x = x + 20

prop_dollarSign :: Int -> Bool
prop_dollarSign x = (f $ x) == f x

prop_dotCompose :: Int -> Bool
prop_dotCompose x = (f . g $ x) == f (g x)

----------------------------------------

prop_foldrConcat :: [Int] -> Bool
prop_foldrConcat l = foldr (:) [] l == (++) [] l

----------------------------------------

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

hm n xs = length (take n xs) == n

prop_hm = forAll (genTuple :: Gen (Int, [Int]))
          (\(x, y) -> hm x y)

----------------------------------------

prop_showRead = forAll (arbitrary :: Gen Int)
                (\x -> (read (show x)) == x)

----------------------------------------

square x = x * x
squareIdentity = square . sqrt

prop_squareIdentity = forAll (arbitrary :: Gen Float)
                      (\x -> x == squareIdentity x) 

----------------------------------------

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord []      = []
capitalizeWord (x: xs) = toUpper x : xs

capitalize x = (capitalizeWord x == twice capitalizeWord x)
            && (capitalizeWord x == fourTimes capitalizeWord x)

prop_idempotence = forAll (arbitrary :: Gen String) capitalize

sorted x = (sort x == twice sort x)
        && (sort x == fourTimes sort x)
prop_idempotenceSorted = forAll (arbitrary :: Gen String) sorted

----------------------------------------

data Fool = Fulse | Frue deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

genFool' :: Gen Fool
genFool' = frequency [(2, return Fulse), (1, return Frue)]

runQc :: IO ()
runQc = do
  quickCheck prop_isAssociative
  quickCheck prop_isCommutative
  quickCheck prop_isAssociativeM
  quickCheck prop_isCommutativeM
  quickCheck prop_isAssociativeP
  quickCheck prop_isCommutativeP
  quickCheck prop_quotRem
  quickCheck prop_divModT
  quickCheck (prop_reverseTwiceIsId  :: [Int] -> Bool)
  quickCheck prop_dotCompose
  quickCheck prop_foldrConcat
  quickCheck prop_hm
  quickCheck prop_showRead
  quickCheck prop_squareIdentity
  quickCheck prop_idempotence
  quickCheck prop_idempotenceSorted

main :: IO ()
main = hspec $ do
  describe "Half" $ do
    it "returns 1 for 2" $ do
      half 2 `shouldBe` 1
    it "returns 0.5 for 1" $ do
      half 1 `shouldBe` 0.5
    it "returns -1 for -2" $ do
      half (-2) `shouldBe` (-1)
    it "returns 0 for 0" $ do
      half 0 `shouldBe` 0

  describe "ListOrdered" $ do
    it "returns True for [1, 2, 2, 3]" $ do
      listOrdered [1, 2, 2, 3] `shouldBe` True
    it "returns False for [1, 3, 2]" $ do
      listOrdered [1, 3, 2] `shouldBe` False
    it "returns True for []" $ do
      listOrdered ([] :: [Int]) `shouldBe` True
    it "returns True for ['a', 'b', 'c']" $ do
      listOrdered ['a', 'b', 'c'] `shouldBe` True
