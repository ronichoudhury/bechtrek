module Data.Bechdel where
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec

-- Used to print data into a standard format.
class Format a where
  format :: a -> String

data Gender = Male | Female | Neither

instance Show Gender where
    show Male = "Male"
    show Female = "Female"
    show Neither = "Neither"

instance Format Gender where
    format Male = "(m)"
    format Female = "(f)"
    format Neither = "(n)"

-- Data type representing a character in a show.
data Role = Role
    { name   :: String
    , gender :: Maybe Gender
    , note   :: Maybe String
    }

instance Show Role where
    show (Role name gender note) = "(" ++ (intercalate " " $ parts) ++ ")"
      where
        parts = ["Role", show name, genderStr, noteStr]
        genderStr = maybe "Nothing" (surroundParen . ("Just "++) . show) gender
        noteStr = maybe "Nothing"  (surroundParen . ("Just "++) . show) note
        surroundParen = ("("++) . (++")")

instance Format Role where
    format role = name role ++ "(" ++ genderStr ++ ")" ++ noteStr
      where
        genderStr = case gender role of
            Just Male ->   "m"
            Just Female -> "f"
            Nothing ->     "u"
        noteStr = maybe "" (\x -> concat ["[", x, "]"]) $ note role

-- Data type representing lines in script.
data ScriptLine = StageDirection String | Scene String | Line Role String

instance Show ScriptLine where
    show (StageDirection dir) = "(StageDirection " ++ show dir ++ ")"
    show (Scene scene) = "(Scene " ++ show scene ++ ")"
    show (Line role line) = "(Line " ++ show role ++ " " ++ show line ++ ")"

instance Format ScriptLine where
    format (StageDirection dir) = intercalate "" ["(", dir, ")"]
    format (Scene scene) = intercalate "" ["[", scene, "]"]
    format (Line role line) = intercalate ": " [format role, line]

-- Parse a role from a string.
parseRole :: GenParser Char () Role
parseRole = do
    name <- manyTill anyChar (lookAhead $ char '(')
    genderLetter <- between (char '(') (char ')') $ oneOf "mfnu"
    note <- optionMaybe $ between (char '[') (char ']') (many $ noneOf "[]")
    return $ Role name (gender genderLetter) note
  where
    gender :: Char -> Maybe Gender
    gender genderLetter = case genderLetter of
        'm' -> Just Male
        'f' -> Just Female
        'n' -> Just Neither
        'u' -> Nothing

-- Read a line of text into a ScriptLine.
parseScriptLine' :: GenParser Char () ScriptLine
parseScriptLine' = try parseLine <|> try parseScene <|> try parseStageDirection

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

parseScriptLine = parse parseScriptLine' "(source)"
