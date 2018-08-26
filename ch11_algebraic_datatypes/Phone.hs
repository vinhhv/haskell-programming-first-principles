module Phone where

import Data.Char
import Data.List
import Data.List.Split

data DaPhone = DaPhone [Button]
phone = DaPhone
  [
    Button '1' "1",
    Button '2' "abc2",
    Button '3' "def3",
    Button '4' "ghi4",
    Button '5' "jkl5",
    Button '6' "mno6",
    Button '7' "pqrs7",
    Button '8' "tuv8",
    Button '9' "wxyz9",
    Button '*' "^*",
    Button '0' " _",
    Button '#' ".,#"
  ]

data Button = Button Char String

convo :: [String]
convo =
  [
    "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am cool Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]

-- Valid buttons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

countPresses :: Char -> String -> Int
countPresses ch choices = go ch choices 0
  where
    go _ [] _ = 0
    go c (x: xs) cnt =
      if c == x then cnt + 1 else go c xs cnt + 1

getTaps :: [Button] -> Char -> [(Digit, Presses)]
getTaps [] ch = []
getTaps ((Button b choices): buttons) ch =
  if any (==ch) choices
  then [(b, countPresses ch choices)]
  else getTaps buttons ch

-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone
            -> Char
            -> [(Digit, Presses)]
reverseTaps (DaPhone layout) ch =
  if isUpper ch
  then ('*', 1) : getTaps layout (toLower ch)
  else getTaps layout (toLower ch)


textToTaps :: DaPhone
               -> String
               -> [(Digit, Presses)]
textToTaps phone = concatMap (reverseTaps phone)

tapsPerText = map (textToTaps phone) convo

-- 3)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldl (\b a -> b + snd a) 0

fingerTapsPerText = map fingerTaps tapsPerText

-- 4)

mostPopularLetter :: String -> (Int, Char)
mostPopularLetter = maximum . map (\xs -> (length xs, head xs)) . group . sort

costOfLetter :: (Int, Char) -> Int
costOfLetter (cnt, ch) = cnt * (fingerTaps $ reverseTaps phone ch)

mostPopular = map mostPopularLetter convo
mostCost = map costOfLetter mostPopular

-- 5)

coolestLtr :: [String] -> (Int, Char)
coolestLtr = maximum . map (foldl (\b a -> (fst b + fst a, snd a)) (0, 'a')) . group . sort. map mostPopularLetter

coolestLtrConvo = coolestLtr convo

coolestWord :: [String] -> (Int, String)
coolestWord = maximum . map (\xs -> (length xs, head xs)) . group . sort . concatMap (splitOn " ")

coolestWordConvo = coolestWord convo
