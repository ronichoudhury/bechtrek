module Data.Bechdel.Util where

-- Replace newlines with spaces.
unnewline :: String -> String
unnewline = map nlToSpace
    where nlToSpace :: Char -> Char
          nlToSpace '\n' = ' '
          nlToSpace c = c
