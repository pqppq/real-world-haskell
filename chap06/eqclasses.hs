data Color = Red | Green | Blue

-- define type clss
class BasicEq a where
  isEqual :: a -> a -> Bool

-- define type
instance BasicEq Bool where
  isEqual True True = True
  isEqual False False = True
  isEqual _ _ = False

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

instance Read Color where
  readsPrec _ value =
    tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
    where
      tryParse [] = []
      tryParse ((attempt, result) : xs) =
        if take (length attempt) value == attempt
          then [(result, drop (length attempt) value)]
          else tryParse xs
