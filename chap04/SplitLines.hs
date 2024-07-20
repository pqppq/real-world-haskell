splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
  -- break :: (a -> Bool) -> [a] -> ([a], [a])
  let (pre, suf) = break isLineTerminator cs
   in -- x : xs, recursive
      pre : case suf of
        ('\r' : '\n' : rest) -> splitLines rest
        ('\r' : rest) -> splitLines rest
        ('\n' : rest) -> splitLines rest
        _ -> []

isLineTerminator c = c == '\r' || c == '\n'
