-- print4.hs
module Print4 where

appendExcl :: String -> String
appendExcl x = x ++ "!"

take5drop4 :: String -> String
take5drop4 x = drop 4 (take 5 x)

drop9 :: String -> String
drop9 x = drop 9 x

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: String -> Int -> Char
letterIndex s i = s !! i

rvrs :: String
rvrs = x
  where s = "Curry is awesome"
        x = concat [(drop 9 s), (take 4 (drop 5 s)), (take 5 s)]

main :: IO()
main = do
  putStrLn (appendExcl "Curry is awesome")
  putStrLn (take5drop4 "Curry is awesome")
  putStrLn (drop9 "Curry is awesome!")
  print $ thirdLetter "howdy yall"
  print $ letterIndex "hello world!" 8
  putStrLn rvrs
