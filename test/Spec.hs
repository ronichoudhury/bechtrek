import Data.Bechdel.Util as U

main :: IO ()
main = do
  print $ U.splitGroups (== 3) [3, 1, 4, 5, 2, 3, 1, 3, 3, 6, 8, 3]
  print $ U.splitGroups (== 3) [2, 1, 4, 5, 2, 3, 1, 3, 3, 6, 8, 3]
