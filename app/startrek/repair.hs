import Control.Monad
import Data.Aeson as A
import Data.Aeson.Encode.Pretty as A
import qualified Data.Bechdel as B
import qualified Data.Bechdel.Script as S
import qualified Data.ByteString.Lazy.Char8 as LT
import Data.Either
import Data.Functor
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import System.Environment
import System.Exit
import System.IO

render :: S.Line -> String
render (S.Dialog role dialog note) = S.name role ++ ": " ++ dialog

-- Ask the user for a name if one is missing.
fillName :: S.Line -> IO S.Line
fillName l@(S.Dialog r@S.Role{S.name="UNKNOWN"} d n) = do
    hPutStrLn stderr $ render l
    hPutStr stderr "What is the role's name? "
    hFlush stderr
    name <- getLine
    return $ S.Dialog r{S.name=name} d n
fillName r = return r

-- Replace unknown names interactively.
replaceNames :: [S.Scene] -> IO [S.Scene]
replaceNames [] = return []
replaceNames (s:ss) = do
    newlines <- mapM fillName (S.lines s)
    (s{S.lines=newlines}:) <$> replaceNames ss

-- Ask the user for a gender if one is missing; use the map to store cached
-- previous answers.
fillGender :: M.Map String S.Gender -> S.Line -> IO (M.Map String S.Gender, S.Line)
fillGender cache l@(S.Dialog r@S.Role{S.gender=Nothing} s n) = do
    case M.lookup (S.name r) cache of
        Just g  -> return (cache, S.Dialog r{S.gender=Just g} s n)
        Nothing -> do
            hPutStrLn stderr $ render l
            hPutStr stderr "What is the role's gender? "
            hFlush stderr
            g <- translate . head <$> getLine
            let newCache = M.insert (S.name r) g cache
            return (newCache, S.Dialog r{S.gender=Just g} s n)
  where
    translate :: Char -> S.Gender
    translate 'm' = S.Male
    translate 'f' = S.Female
    translate 'o' = S.Other
    translate c = error $ "Gender must be one of 'm', 'f', or 'o' (was '" ++ show c ++ "')"
fillGender cache l@(S.Dialog r@S.Role{S.gender=Just g} s n) = return (M.insert (S.name r) g cache, l)
fillGender cache l = return (cache, l)

replaceGenders :: [S.Scene] -> IO [S.Scene]
replaceGenders = go (M.fromList [])
  where
    go :: M.Map String S.Gender -> [S.Scene] -> IO [S.Scene]
    go _ [] = return []
    go cache (s:ss) = do
        (newcache, newlines) <- fillGenders cache $ S.lines s
        rest <- go newcache ss
        return $ s{S.lines=newlines} : rest

    fillGenders :: M.Map String S.Gender -> [S.Line] -> IO (M.Map String S.Gender, [S.Line])
    fillGenders cache [] = return (cache, [])
    fillGenders cache (l:ll) = do
        (newcache, newline) <- fillGender cache l
        (newercache, rest) <- fillGenders newcache ll
        return $ (newercache, newline : rest)

-- Apply fillGender to a list of ScriptLines, threading the evolving cache
-- through the invocations.
--
-- TODO: replace this one-off mechanism with a use of StateT monad.
fillGenders :: [S.Line] -> IO [S.Line]
fillGenders = go (M.fromList [])
  where
    go :: M.Map String S.Gender -> [S.Line] -> IO [S.Line]
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
    text <- readFile (head args)

    -- Parse the JSON.
    let parse = A.eitherDecode $ (T.encodeUtf8 . T.pack) text :: Either String S.Script
    when (isLeft parse) $ do
        hPutStrLn stderr $ "error parsing JSON: " ++ (fromLeft undefined parse)
        exitFailure

    let script = fromRight undefined parse

    -- Find script lines with characters with unknown names and genders, and
    -- fill them in interactively.
    namedScenes <- replaceNames (S.scenes script)
    genderedScenes <- replaceGenders namedScenes
    let filledScript = script{S.scenes=genderedScenes}

    -- Dump the result to JSON.
    LT.putStrLn $ S.dumps filledScript
