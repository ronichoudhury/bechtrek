import Control.Monad
import Data.Aeson as A
import Data.Aeson.Encode.Pretty as A
import Data.Bechdel.Script as S
import Data.Either
import Data.Functor
import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Typeable
import System.Environment
import System.Exit
import System.IO

-- Female (and, consequently, line) detector, for use in counting the number of
-- female characters in a scene.
isFemale :: S.Line -> Bool
isFemale (S.Dialog r@S.Role{S.gender=Just S.Female} _ _) = True
isFemale _ = False

-- Extract a role from a ScriptLine (dangerous function but we only call it on a
-- list that is known to contain only Line objects).
getRole :: S.Line -> S.Role
getRole (Dialog r _ _) = r
getRole _ = error "impossible"

-- Counter for number of distinct, female roles in a scene.
countDistinct :: [S.Line] -> Int
countDistinct lines = countDistinct' lines Set.empty 0
  where
    countDistinct' :: [S.Line] -> Set.Set String -> Int -> Int
    countDistinct' [] _ c = c
    countDistinct' (x:xs) s c =
        if isFemale x && Set.notMember rolename s
            then countDistinct' xs (Set.insert rolename s) (c + 1)
            else countDistinct' xs s c
      where
        rolename = name . role $ x

-- This function filters away all scenes that have fewer than two female
-- characters, and asks the user whether it passes the Bechdel test.
askBechdel :: [S.Line] -> IO Bool
askBechdel scene = do
    let numFemale = countDistinct $ filter isFemale scene
    if numFemale < 2
        then return False
        else do
            mapM_ (hPutStrLn stderr . show) scene
            hPutStr stderr "\nDoes this scene pass the Bechdel test? "
            hFlush stderr
            answer <- getLine
            hPutStrLn stderr ""
            return $ null answer || case head answer of
                'y' -> True
                otherwise -> False

main :: IO ()
main = do
    -- Read a script in from the first argument.
    args <- getArgs
    when (null args) $ do
        hPutStrLn stderr "usage: bechdel <scriptfile>"
        exitFailure
    text <- readFile $ head args

    -- Parse the JSON.
    let parse = A.eitherDecode $ (T.encodeUtf8 . T.pack) text :: Either String S.Script
    when (isLeft parse) $ do
        hPutStrLn stderr $ "error parsing JSON: " ++ (fromLeft undefined parse)
        exitFailure

    let script = fromRight undefined parse

    -- Split the script lines into groupings headed by a Scene.
    let lines = map S.lines $ S.scenes script

    -- Ask the user to tell if each scene passes Bechdel or not.
    results <- mapM askBechdel lines

    -- Add up the results.
    let score = sum $ map (\x -> if x then 1 else 0) results
    print score
