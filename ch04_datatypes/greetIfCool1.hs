module GreetIfCool1 where

greetIfCool :: String -> IO()
greetIfCool coolness =
  if cool
    then putStrLn "eyyyyy. What's shakin'?"
  else
    putStrLn "pshhhhhh."
  where cool =
          coolness == "downright frosty yo"
