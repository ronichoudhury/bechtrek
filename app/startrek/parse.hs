{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.Aeson as A
import Data.Aeson.Encode.Pretty as A
import Data.Bechdel as B
import Data.Bechdel.Script as S
import Data.Bechdel.Util
import qualified Data.ByteString.Lazy.Char8 as LT
import Data.Either
import Data.Functor
import Data.List
import Data.List.Split
import Data.Maybe
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
        return $ B.StageDirection text

    sceneParser = do
        text <- between (char '[') (char ']') (many $ noneOf "]")
        return $ B.Scene text

    lineParser = do
        role <- parseRawRole
        char ':'
        spaces
        line <- many anyChar
        return $ Line role line

    logParser = do
        text <- many anyChar
        if isLog text
            then return $ B.Line (B.Role "UNKNOWN" Nothing Nothing) text
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
        return $ B.Role (strip name) Nothing note

-- Parse HTML text.
parseHTML = readString [withParseHTML yes, withWarnings no]

-- Get the episode title from the HTML.
extractScriptTitle html = do
    titleText <- head <$> (runX $ html >>> css "title" //> getText)
    return $ strip . unnewline . intercalate "-" $ (tail $ splitOn "-" titleText)

-- Extract all appropriate text nodes comprising script.
extractScriptText html = runX $ html >>> css "table font" //> getText

-- Convenience function.
readHTMLScript = extractScriptText . parseHTML

report :: Handle -> Either ParseError ScriptLine -> IO Bool
report handle (Left parseError) = do
    hPutStrLn handle "ERROR-------"
    hPutStrLn handle $ show parseError
    return False
report handle (Right result) = do
    hPutStrLn handle $ format result
    {-hPutStrLn handle $ show result-}
    return True

isScene :: ScriptLine -> Bool
isScene (B.Scene _) = True
isScene _ = False

convertJSON :: [ScriptLine] -> LT.ByteString
convertJSON lines = json
  where
    all = splitGroups isScene lines
    title = head all
    scenes = tail all
    sceneObj = map aggregate scenes

    config = let spaces = Spaces 2
                 order = (A.keyOrder ["role", "name", "gender", "note", "sceneDescription", "series", "title", "season", "episode"])
                 format = confNumFormat A.defConfig
                 trailing = confTrailingNewline A.defConfig
             in Config spaces order format trailing
    json = A.encodePretty' config sceneObj

    aggregate :: [B.ScriptLine] -> S.Scene
    aggregate ((B.Scene desc):lines) = S.Scene desc $ map convertLine lines
    aggregate x = error $ "<<<" ++ show x ++ ">>>"

    convertLine :: B.ScriptLine -> S.Line
    convertLine (B.Line role line) = S.Dialog (S.Role (B.name role) (convertGender <$> B.gender role)) line (B.note role)
    convertLine (B.StageDirection d) = S.StageDirection d

    convertGender :: B.Gender -> S.Gender
    convertGender B.Male = S.Male
    convertGender B.Female = S.Female
    convertGender B.Neither = S.Other

edit :: Either ParseError ScriptLine -> String -> IO String
edit (Left err) line = do
    (path, h) <- openTempFile "/tmp" "bechedit.txt"
    hPutStrLn h line
    hPutStrLn h $ ""
    hPutStrLn h $ "##########"
    hPutStrLn h $ "Edit the first line of this file to correct the parsing error\n"
    hPutStrLn h $ "Write just the word \"join\" to join this line of text in its entirety to the previous line.\n"
    hPutStrLn h $ "Write just the word \"exit\" to abandon processing this script.\n"
    hPutStrLn h $ "----------"
    hPutStrLn h $ "The parse error was:"
    hPutStrLn h $ show err
    hClose h

    callCommand $ "vim " ++ path

    hh <- openFile path ReadMode
    modified <- hGetLine hh
    hClose hh
    removeFile path

    when (modified == "exit") $ do
        hPutStrLn stderr $ "exiting"
        exitFailure

    let join = modified == "join"
    let prefix = if join then "join: " else ""

    return $ prefix ++ (if join then line else modified)

-- Parse the argument; if the parse fails, launch an editor to let the user
-- correct the parse error, and loop until the string is corrected.
parseLineWithCorrection :: String -> IO String
parseLineWithCorrection line = do
    let parsed = parseRawLine line
    if (isLeft parsed)
        then edit parsed line >>= parseLineWithCorrection
        else return line

-- Join all "join" lines to the preceding line.
stitch :: [String] -> [String]
stitch [] = []
stitch (s:ss) = stitch' s ss
  where
    stitch' :: String -> [String] -> [String]
    stitch' last [] = [last]
    stitch' last (s:ss)
      | "join:" `isPrefixOf` s = stitch' (last ++ (fromJust . stripPrefix "join:" $ s)) ss
      | otherwise = last : stitch' s ss

-- Main function: open the file, read its contents, parse into ScriptLines, and
-- spit them back onto stdout.
main :: IO ()
main = do
    args <- getArgs
    when (length args < 2) $ do
        hPutStrLn stderr "usage: parse <scriptfile> <outputfile>"
        exitFailure

    -- Open the input file for reading; grab its contents.
    file <- openFile (head args) ReadMode
    hSetEncoding file latin1
    text <- hGetContents file

    -- Open the output file for writing.
    out <- openFile (args !! 1) WriteMode

    -- Open the file, read its contents, and parse out the script lines from the
    -- HTML.
    title <- extractScriptTitle . parseHTML $ text
    script <- readHTMLScript text

    -- Extract the appropriate tags from the text.
    lines <- stitch <$> (sequence . map parseLineWithCorrection . filter (not . null) . map (strip . unnewline) $ script)

    -- Parse a script from the text, prepending a title record.
    let scriptLines = (Right $ Title title) : map parseRawLine lines

    LT.putStrLn $ convertJSON (rights scriptLines)

    -- Print out the script in standard format.
    good <- mapM (report out) $ scriptLines
    if all (== True) good
        then exitSuccess
        else exitFailure
