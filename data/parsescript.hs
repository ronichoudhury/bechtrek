import Control.Monad
import Data.Functor
import Data.String.Utils
import Prelude hiding (readFile, putStrLn, print)
import System.Environment
import System.Exit
import System.IO.UTF8
import System.IO (stderr)
import Text.XML.HXT.Core hiding (when)
import Text.HandsomeSoup

-- Replace newlines with spaces.
unnewline :: String -> String
unnewline = map nlToSpace
    where nlToSpace :: Char -> Char
          nlToSpace '\n' = ' '
          nlToSpace c = c

unnewline' :: String -> String
unnewline' "" = ""
unnewline' ('\n':cs) = ' ':(unnewline cs)
unnewline' (c:cs) = c:(unnewline cs)

main :: IO ()
main = do
    -- Process command line arguments.
    args <- getArgs
    when (null args) $ do
        hPutStrLn stderr "usage: parsescript <scriptfile>"
        exitFailure

    -- Open the file and read its contents.
    text <- readFile $ head args

    -- Parse the HTML out.
    let doc = readString [withParseHTML yes, withWarnings no] text

    -- Extract the appropriate tags from the text.
    dialogue <- runX $ doc >>> css "table p font" //> getText
    mapM_ putStrLn $ map (strip . unnewline) dialogue

    -- Yay!
    exitSuccess
