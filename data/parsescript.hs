import Control.Monad
import Data.Functor
import Data.List
import Data.List.Split
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

-- Data type representing lines in script.
data ScriptLine = StageDirection String | Scene String | Line String String

instance Show ScriptLine where
    show (StageDirection dir) = intercalate "" ["(", dir, ")"]
    show (Scene scene) = intercalate "" ["[", scene, "]"]
    show (Line role line) = intercalate ": " [role, line]

parseRawLine :: String -> ScriptLine
parseRawLine line@(c:_)
    | c == '['  = Scene $ contents line
    | c == '('  = StageDirection $ contents line
    | otherwise = Line (head parsedLine) (intercalate ":" $ tail parsedLine)
        where parsedLine = if length candidates > 0
                               then head candidates
                               else ["UNKNOWN", line]
                  where colon = splitOn ": " line
                        semicolon = splitOn "; " line
                        candidates = filter (\x -> length x > 1) [colon, semicolon]
              contents = init . tail

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
    mapM_ print . (map parseRawLine) . (filter $ not . null) . (map $ strip . unnewline) $ dialogue

    -- Yay!
    exitSuccess
