import Control.Monad
import Data.Bechdel
import Data.Bechdel.Util
import Data.Either
import Data.Functor
import Data.List
import Data.List.Split
import Data.String.Utils
import Debug.Trace
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.XML.HXT.Core hiding (when, trace)
import Text.HandsomeSoup
import Text.ParserCombinators.Parsec

-- Parse a script line from a line of text.
parseRawLine :: String -> Either ParseError ScriptLine
parseRawLine s = parse parser s s
  where
    parser = try stagedirParser <|> try sceneParser <|> try lineParser <|> try logParser

    stagedirParser = try stagedirParserWhole

    stagedirParserWhole = do
        char '('
        text <- manyTill anyChar (try $ lookAhead (char ')' >> spaces >> eof))
        return $ StageDirection text

    sceneParser = do
        text <- between (char '[') (char ']') (many $ noneOf "]")
        return $ Scene text

    lineParser = do
        role <- parseRawRole
        char ':'
        spaces
        line <- many anyChar
        return $ Line role line

    logParser = do
        text <- many anyChar
        if isLog text
            then return $ Line (Role "UNKNOWN" Nothing Nothing) text
            else fail "Could not parse"
      where
        isLog s = any (== True) $ map ($s) [isInfixOf "Star date",
                                            isInfixOf "Stardate",
                                            isInfixOf "log",
                                            isInfixOf "Log"]

    parseRawRole = do
        name <- many $ noneOf ":["
        spaces
        note <- optionMaybe $ between (char '[') (char ']') (many $ noneOf "]")
        return $ Role name Nothing note

-- Parse HTML text.
parseHTML = readString [withParseHTML yes, withWarnings no]

-- Extract all appropriate text nodes comprising script.
extractScriptText html = runX $ html >>> css "table p font" //> getText

-- Convenience function.
readHTMLScript = extractScriptText . parseHTML

report :: Either ParseError ScriptLine -> IO Bool
report (Left parseError) = do
    hPutStrLn stderr "ERROR-------"
    hPutStrLn stderr $ show parseError
    hFlush stderr
    return False
report (Right result) = do
    putStrLn $ format result
    return True

edit :: Either ParseError ScriptLine -> String -> IO String
edit (Left err) line = do
    (path, h) <- openTempFile "/tmp" "bechedit.txt"
    hPutStrLn h line
    hPutStrLn h $ ""
    hPutStrLn h $ "##### Edit the first line of this file to correct the parsing error #####"
    hPutStrLn h $ "PARSE ERROR:"
    hPutStrLn h $ show err
    hClose h

    callCommand $ "vim " ++ path

    hh <- openFile path ReadMode
    modified <- hGetLine hh
    hClose hh
    removeFile path

    return modified

-- Parse the argument; if the parse fails, launch an editor to let the user
-- correct the parse error, and loop until the string is corrected.
parseLineWithCorrection :: String -> IO String
parseLineWithCorrection line = do
    let parsed = parseRawLine line
    if (isLeft parsed)
        then edit parsed line >>= parseLineWithCorrection
        else return line

-- Main function: open the file, read its contents, parse into ScriptLines, and
-- spit them back onto stdout.
main :: IO ()
main = do
    args <- getArgs
    when (null args) $ do
        hPutStrLn stderr "usage: parsechakoteya <scriptfile>"
        exitFailure
    file <- openFile (head args) ReadMode
    hSetEncoding file latin1
    text <- hGetContents file

    -- Open the file, read its contents, and parse out the script lines from the
    -- HTML.
    script <- readHTMLScript text


    -- Extract the appropriate tags from the text.
    lines <- sequence . map parseLineWithCorrection . filter (not . null) . map (strip . unnewline) $ script

    let scriptLines = map parseRawLine lines

    -- Print out the script in standard format.
    good <- mapM report $ scriptLines
    if all (== True) good
        then exitSuccess
        else exitFailure
