import Control.Monad
import Data.Bechdel
import Data.Either
import Data.Functor
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

    mapM_ (putStrLn . format) namedScript

    exitSuccess
