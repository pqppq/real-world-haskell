main = do
  putStrLn "Please enter a Double:"
  input <- getLine
  let double = read input :: Double
  putStrLn ("Twise " ++ show double ++ " is " ++ show (double * 2))
