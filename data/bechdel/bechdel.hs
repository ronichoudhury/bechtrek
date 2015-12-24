import Control.Monad
import Data.Bechdel
import Data.Either
import Data.Functor
import Data.List.Split
import qualified Data.Set as S
import Data.Typeable
import System.Environment
import System.Exit
import System.IO

-- Scene detector, for use in chunking the script up into scenes.
isScene :: ScriptLine -> Bool
isScene (Scene{}) = True
isScene _ = False

-- Female (and, consequently, line) detector, for use in counting the number of
-- female characters in a scene.
isFemale :: ScriptLine -> Bool
isFemale (Line r@Role{gender=Just Female} _) = True
isFemale _ = False

-- Extract a role from a ScriptLine (dangerous function but we only call it on a
-- list that is known to contain only Line objects).
role :: ScriptLine -> Role
role (Line r _) = r

-- Counter for number of distinct, female roles in a scene.
countDistinct :: [ScriptLine] -> Int
countDistinct lines = countDistinct' lines S.empty 0
  where
    countDistinct' :: [ScriptLine] -> S.Set String -> Int -> Int
    countDistinct' [] _ c = c
    countDistinct' (x:xs) s c =
        if isFemale x && S.notMember rolename s
            then countDistinct' xs (S.insert rolename s) (c + 1)
            else countDistinct' xs s c
      where
        rolename = name . role $ x

-- This function filters away all scenes that have fewer than two female
-- characters, and asks the user whether it passes the Bechdel test.
askBechdel :: [ScriptLine] -> IO Bool
askBechdel scene = do
    let numFemale = countDistinct $ filter isFemale scene
    if numFemale < 2
        then return False
        else do
            mapM_ (putStrLn . format) scene
            putStr "\nDoes this scene pass the Bechdel test? "
            hFlush stdout
            answer <- getLine
            putStrLn ""
            return $ (null answer) || case head answer of
                'y' -> True
                otherwise -> False

main :: IO ()
main = do
    -- Read a script in from the first argument.
    args <- getArgs
    when (null args) $ do
        hPutStrLn stderr "usage: bechdel <scriptfile>"
        exitFailure
    text <- lines <$> readFile (head args)

    -- Parse into ScriptLines
    let scriptParse = map parseScriptLine text

    -- Check for well-formedness of the input.
    let bad = filter (isLeft . fst) $ zip scriptParse text
    unless (null bad) $ do
        hPutStrLn stderr "Error - badly formatted script line:"
        hPutStrLn stderr (snd . head $ bad)
        exitFailure

    -- Extract the actual ScriptLine objects.
    let script = rights scriptParse

    -- Split the script lines into groupings headed by a Scene.
    let scenes = split (keepDelimsL $ whenElt isScene) script

    -- Ask the user to tell if each scene passes Bechdel or not.
    results <- mapM askBechdel scenes

    -- Add up the results.
    let score = sum $ map (\x -> if x then 1 else 0) results
    print score
