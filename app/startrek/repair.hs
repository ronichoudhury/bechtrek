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

-- Replace scenes
replaceScenes :: [S.Scene] -> (S.Line -> IO S.Line) -> IO [S.Scene]
replaceScenes [] _ = return []
replaceScenes (s:ss) f = do
    newlines <- mapM f (S.lines s)
    (s{S.lines=newlines}:) <$> replaceScenes ss f

-- Ask the user for a gender if one is missing; use the map to store cached
-- previous answers.
{-fillGender :: M.Map String Gender -> ScriptLine -> IO (M.Map String Gender, ScriptLine)-}
{-fillGender cache l@(Line r@Role{gender=Nothing} s) = do-}
    {-case M.lookup (name r) cache of-}
        {-Just g  -> return (cache, Line r{gender=Just g} s)-}
        {-Nothing -> do-}
            {-hPutStrLn stderr $ format l-}
            {-hPutStr stderr "What is the role's gender? "-}
            {-hFlush stderr-}
            {-g <- translate . head <$> getLine-}
            {-let newCache = M.insert (name r) g cache-}
            {-return (newCache, Line r{gender=Just g} s)-}
  {-where-}
    {-translate :: Char -> Gender-}
    {-translate 'm' = Male-}
    {-translate 'f' = Female-}
    {-translate 'n' = Neither-}
    {-translate c = error $ "Illegal character for gender: " ++ show c-}
{-fillGender cache l@(Line r@Role{gender=Just g} s) = return (M.insert (name r) g cache, l)-}
{-fillGender cache l = return (cache, l)-}

-- Apply fillGender to a list of ScriptLines, threading the evolving cache
-- through the invocations.
--
-- TODO: replace this one-off mechanism with a use of StateT monad.
{-fillGenders :: [ScriptLine] -> IO [ScriptLine]-}
{-fillGenders = go (M.fromList [])-}
  {-where-}
    {-go :: M.Map String Gender -> [ScriptLine] -> IO [ScriptLine]-}
    {-go _ [] = return []-}
    {-go cache (x:xs) = do-}
        {-result <- fillGender cache x-}
        {-let newCache = fst result-}
        {-let value = snd result-}
        {-rest <- go newCache xs-}
        {-return $ value : rest-}

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

    -- Find script lines with characters with unknown names.
    namedScenes <- replaceScenes (S.scenes script) fillName
    let filledScript = script{S.scenes=namedScenes}

    -- Find script lines with characters with unknown gender.
    {-genderedScenes <- replaceScenes namedScenes fillGender-}
    {-let filledScript = script{S.scenes=genderedScenes}-}

    LT.putStrLn $ S.dumps filledScript
