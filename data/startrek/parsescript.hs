import Control.Monad
import Data.Bechdel
import Data.Bechdel.Util
import Data.Functor
import Data.List
import Data.String.Utils
import Prelude hiding (readFile, putStrLn, print)
import System.Environment
import System.Exit
import System.IO.UTF8
import System.IO (stderr)

main :: IO ()
main = do
    -- Process command line arguments.
    args <- getArgs
    when (null args) $ do
        hPutStrLn stderr "usage: parsescript <scriptfile>"
        exitFailure

    -- Open the file and read its contents.
    text <- readFile $ head args

    -- Read out the script text from the file.
    let script = readHTMLScript text

    -- Extract the appropriate tags from the text.
    scriptLines <- map parseRawLine . filter (not . null) . map (strip . unnewline) <$> script

    mapM_ print scriptLines

    -- Yay!
    exitSuccess
