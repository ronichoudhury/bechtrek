import Control.Monad
import Data.Bechdel
import Data.Either
import Data.Functor
import qualified Data.Map as M
import System.Environment
import System.Exit
import System.IO

-- Ask the user for a name if one is missing.
fillName :: ScriptLine -> IO ScriptLine
fillName l@(Line r@Role{name="UNKNOWN"} s) = do
    hPutStrLn stderr $ format l
    hPutStr stderr "What is the role's name? "
    hFlush stderr
    name <- getLine
    return $ Line r{name=name} s
fillName r = return r

-- Get a legal gender character
getGenderChar :: IO Char
getGenderChar = do
  c <- head <$> getLine
  if valid c
    then return c
    else do
      hPutStr stderr $ "Illegal character for gender " ++ show c ++ " (should be m/f/n) - try again: "
      getGenderChar
 where
  valid c = c `elem` "mfn"

-- Ask the user for a gender if one is missing; use the map to store cached
-- previous answers.
fillGender :: M.Map String Gender -> ScriptLine -> IO (M.Map String Gender, ScriptLine)
fillGender cache l@(Line r@Role{gender=Nothing} s) = do
    case M.lookup (name r) cache of
        Just g  -> return (cache, Line r{gender=Just g} s)
        Nothing -> do
            hPutStrLn stderr $ format l
            hPutStr stderr "What is the role's gender? "
            hFlush stderr
            g <- translate <$> getGenderChar
            let newCache = M.insert (name r) g cache
            return (newCache, Line r{gender=Just g} s)
  where
    translate :: Char -> Gender
    translate 'm' = Male
    translate 'f' = Female
    translate 'n' = Neither
    translate c = error $ "Illegal character for gender: " ++ show c
fillGender cache l@(Line r@Role{gender=Just g} s) = return (M.insert (name r) g cache, l)
fillGender cache l = return (cache, l)

-- Apply fillGender to a list of ScriptLines, threading the evolving cache
-- through the invocations.
--
-- TODO: replace this one-off mechanism with a use of StateT monad.
fillGenders :: [ScriptLine] -> IO [ScriptLine]
fillGenders = go (M.fromList [])
  where
    go :: M.Map String Gender -> [ScriptLine] -> IO [ScriptLine]
    go _ [] = return []
    go cache (x:xs) = do
        result <- fillGender cache x
        let newCache = fst result
        let value = snd result
        rest <- go newCache xs
        return $ value : rest

main :: IO ()
main = do
    -- Read a script in from the file named in the first argument.
    args <- getArgs
    when (null args) $ do
        hPutStrLn stderr "usage: repair <scriptfile>"
        exitFailure
    text <- lines <$> readFile (head args)

    -- Parse into ScriptLines
    let scriptParse = map parseScriptLine text

    -- Check for well formedness of the result.
    let bad = filter (isLeft . fst) $ zip scriptParse text
    unless (null bad) $ do
        hPutStrLn stderr "Error - badly formatted script line:"
        hPutStrLn stderr (snd . head $ bad)
        exitFailure

    let script = rights scriptParse

    -- Find script lines with characters with unknown names.
    namedScript <- mapM fillName script

    -- Find script lines with characters with unknown gender.
    genderedNamedScript <- fillGenders namedScript

    mapM_ (putStrLn . format) genderedNamedScript

    exitSuccess
