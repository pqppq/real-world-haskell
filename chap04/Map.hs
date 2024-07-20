import Data.Char (toUpper)

upperCase :: String -> String
upperCase = map toUpper

square2 = map (\x -> x * x)

myMap :: (a -> b) -> [a] -> [b]
myMap f (x : xs) = f x : myMap f xs
myMap _ _ = []
