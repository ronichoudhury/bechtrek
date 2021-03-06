module Data.Bechdel where
import Data.List
import Data.Maybe
import Data.String.Utils
import Text.ParserCombinators.Parsec

-- Used to print data into a standard format. The boolean parameter controls
-- whether the formatting is done using color or not.
class Format a where
  rawformat :: Bool -> a -> String

  format :: a -> String
  format = rawformat False

  cformat :: a -> String
  cformat = rawformat True

data Gender = Male | Female | Neither deriving Eq

instance Show Gender where
    show Male = "Male"
    show Female = "Female"
    show Neither = "Neither"

instance Format Gender where
    rawformat _ Male = "(m)"
    rawformat _ Female = "(f)"
    rawformat _ Neither = "(n)"

-- Data type representing a character in a show.
data Role = Role
    { name   :: String
    , gender :: Maybe Gender
    , note   :: Maybe String
    }

instance Show Role where
    show (Role name gender note) = "(" ++ unwords parts ++ ")"
      where
        parts = ["Role", show name, genderStr, noteStr]
        genderStr = maybe "Nothing" (surroundParen . ("Just "++) . show) gender
        noteStr = maybe "Nothing"  (surroundParen . ("Just "++) . show) note
        surroundParen = ("("++) . (++")")

instance Format Role where
    rawformat color role = colorStr ++ name role ++ "(" ++ genderStr ++ ")" ++ noteStr ++ clearStr
      where
        genderStr = case gender role of
            Just Male ->   "m"
            Just Female -> "f"
            Just Neither -> "n"
            Nothing ->     "u"
        noteStr = maybe "" (\x -> concat ["[", x, "]"]) $ note role
        colorStr =
            if color && gender role == Just Female
                then "\x1b[1;91m"
                else ""
        clearStr = if color then "\x1b[0m" else ""

-- Data type representing lines in script.
data ScriptLine = StageDirection String | Scene String | Line Role String | Title String

instance Show ScriptLine where
    show (StageDirection dir) = "(StageDirection " ++ show dir ++ ")"
    show (Scene scene) = "(Scene " ++ show scene ++ ")"
    show (Line role line) = "(Line " ++ show role ++ " " ++ show line ++ ")"
    show (Title title) = "(Title " ++ show title ++ ")"

instance Format ScriptLine where
    rawformat _ (StageDirection dir) = intercalate "" ["(", dir, ")"]
    rawformat color (Scene scene) = intercalate "" [colorStr, "[", scene, "]", clearStr]
      where
        colorStr = if color then "\x1b[1;94m" else ""
        clearStr = if color then "\x1b[0m" else ""
    rawformat color (Line role line) = intercalate ": " [rawformat color role, line]
    rawformat color (Title title) = colorStr ++ "========== " ++ title ++ clearStr
      where
        colorStr = if color then "\x1b[1;91m" else ""
        clearStr = if color then "\x1b[0m" else ""

-- Parse a role from a string.
parseRole :: GenParser Char () Role
parseRole = do
    name <- manyTill anyChar (lookAhead $ char '(')
    genderLetter <- between (char '(') (char ')') $ oneOf "mfnu"
    note <- optionMaybe $ between (char '[') (char ']') (many $ noneOf "[]")
    return $ Role (strip name) (gender genderLetter) note
  where
    gender :: Char -> Maybe Gender
    gender genderLetter = case genderLetter of
        'm' -> Just Male
        'f' -> Just Female
        'n' -> Just Neither
        'u' -> Nothing

-- Read a line of text into a ScriptLine.
parseScriptLine' :: GenParser Char () ScriptLine
parseScriptLine' = try parseLine <|> try parseScene <|> try parseStageDirection <|> try parseTitle

-- Parse a ScriptLine out of a line of dialogue.
parseLine :: GenParser Char () ScriptLine
parseLine = do
    role <- parseRole
    char ':'
    spaces
    dialogue <- many anyChar
    return $ Line role dialogue

-- Parse a ScriptLine out of a stage direction.
parseStageDirection :: GenParser Char () ScriptLine
parseStageDirection = do
    char '('
    text <- manyTill anyChar (try $ lookAhead (char ')' >> spaces >> eof))
    return $ StageDirection text

parseScene :: GenParser Char () ScriptLine
parseScene = do
    text <- between (char '[') (char ']') (many $ noneOf "[]")
    return $ Scene text

-- Parse a ScriptLine out of a title.
parseTitle :: GenParser Char () ScriptLine
parseTitle = do
    string "========== "
    title <- many1 anyChar
    return $ Title title

parseScriptLine = parse parseScriptLine' "(source)"
