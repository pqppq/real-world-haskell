name2reply :: String -> String
name2reply name =
  "Pleased to meet you, "
    ++ name
    ++ ".\n"
    ++ "Your name contains "
    ++ charcount
    ++ " characters."
  where
    charcount = show (length name)

main :: IO ()
main = do
  putStrLn "Greetings once again. What is your name?"
  input <- getLine
  let output = name2reply input
  putStrLn output
