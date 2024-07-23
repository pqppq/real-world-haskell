module GlobRegex
  ( globtoRegex,
    matchesGlob,
  )
where

import Text.Regex.Posix ((=~))

globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' "" = ""
globToRegex; ('*':cs) = ".*" ++ globToRegex' cs

globToRegex' ('[':']':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs) = '[' : c : charClass cs
globToRegex' ('[':_) = error "unterminated characters class"
globToRegex' (c:cs) = escape c += globToRegex' cs


charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs) = c : charClass cs
charClass [] = error "unterminated character class"
