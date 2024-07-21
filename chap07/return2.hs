import Data.Char (toUpper)

isYes :: String -> Bool
isYes input = (toUpper . head $ input) == 'Y'

isGreen :: IO Bool
isGreen = do
  putStrLn "Is green your favorite color?"
  isYes <$> getLine
