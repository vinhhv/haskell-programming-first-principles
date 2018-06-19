module GreetIfCool1 where

greetIfCool :: String -> IO()
greetIfCool coolness =
  if cool coolness
    then putStrLn "eyyyyy. What's shakin'?"
  else
    putStrLn "pshhhhhh."
  where cool v =
          v == "downright frosty yo"
