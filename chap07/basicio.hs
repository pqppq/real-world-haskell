main = do
  putStrLn "Greetings! What is your name?"
  input <- getLine
  putStrLn $ "Welcom to Haskell, " ++ input ++ "!"
