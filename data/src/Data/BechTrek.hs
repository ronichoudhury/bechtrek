module Data.BechTrek where
import Data.List
import Data.List.Split
import Text.XML.HXT.Core hiding (when)
import Text.HandsomeSoup

-- Data type representing lines in script.
data ScriptLine = StageDirection String | Scene String | Line String String

instance Show ScriptLine where
    show (StageDirection dir) = intercalate "" ["(", dir, ")"]
    show (Scene scene) = intercalate "" ["[", scene, "]"]
    show (Line role line) = intercalate ": " [role, line]

-- Parse a script line from a line of text.
parseRawLine :: String -> ScriptLine
parseRawLine line@(c:_)
    | c == '['  = Scene $ contents line
    | c == '('  = StageDirection $ contents line
    | otherwise = Line (head parsedLine) (intercalate ":" $ tail parsedLine)
        where parsedLine = if not (null candidates)
                               then head candidates
                               else ["UNKNOWN", line]
                  where colon = splitOn ": " line
                        semicolon = splitOn "; " line
                        candidates = filter (\x -> length x > 1) [colon, semicolon]
              contents = init . tail

-- Parse HTML text.
parseHTML = readString [withParseHTML yes, withWarnings no]

-- Extract all appropriate text nodes comprising script.
extractScriptText html = runX $ html >>> css "table p font" //> getText

-- Convenience function.
readHTMLScript = extractScriptText . parseHTML
