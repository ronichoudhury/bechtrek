module Data.Bechdel.Util where

-- Replace newlines with spaces.
unnewline :: String -> String
unnewline = map nlToSpace
    where nlToSpace :: Char -> Char
          nlToSpace '\n' = ' '
          nlToSpace c = c

-- Split a list into sublists by a predicate.
splitGroups :: (a -> Bool) -> [a] -> [[a]]
splitGroups _ [] = []
splitGroups f xs = splitGroups' [] [] f xs
  where
    splitGroups' :: [[a]] -> [a] -> (a -> Bool) -> [a] -> [[a]]
    splitGroups' accum current _ [] = reverse $ current:accum
    splitGroups' accum current f (x:xs) = if f x
        then splitGroups' ((reverse current):accum) [x] f xs
        else splitGroups' accum (x:current) f xs
