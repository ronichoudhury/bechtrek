import Control.Monad
import Data.Bechdel
import Data.Bechdel.Util
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

-- Parse a script line from a line of text.
parseRawLine :: String -> ScriptLine
parseRawLine line@(c:_)
    | c == '['  = Scene $ contents line
    | c == '('  = StageDirection $ contents line
    | otherwise = Line (parseRole $ head parsedLine) (intercalate ":" $ tail parsedLine)
        where parsedLine = if not (null candidates)
                               then head candidates
                               else ["UNKNOWN", line]
                  where colon = splitOn ": " line
                        semicolon = splitOn "; " line
                        candidates = filter (\x -> length x > 1) [colon, semicolon]
              contents = init . tail

-- Parse a string into a role.
parseRole :: String -> Role
parseRole s = Role name Nothing note
  where
    name = head parts
    note = case length parts of
        1 -> Nothing
        _ -> Just $ init . head . tail $ parts
    parts = splitOn " [" s

-- Parse HTML text.
parseHTML = readString [withParseHTML yes, withWarnings no]

-- Extract all appropriate text nodes comprising script.
extractScriptText html = runX $ html >>> css "table p font" //> getText

-- Convenience function.
readHTMLScript = extractScriptText . parseHTML

-- Main function: open the file, read its contents, parse into ScriptLines, and
-- spit them back onto stdout.
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
