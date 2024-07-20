import Data.List

suffixes :: [a] -> [[a]]
suffixes xs@(_ : xs') = xs : suffixes xs'
suffixes _ = []

noAsPattern :: [a] -> [[a]]
noAsPattern (x : xs) = (x : xs) : noAsPattern xs
noAsPattern _ = []

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

suffixes2 xs = init  (tails xs)
