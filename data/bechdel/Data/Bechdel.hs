module Data.Bechdel where
import Data.List
import Data.Maybe

class Repr a where
  repr :: a -> String

data Gender = Male | Female | Neither

instance Show Gender where
    show Male = "male"
    show Female = "female"
    show Neither = "neither"

-- Data type representing a character in a show.
data Role = Role
    { name   :: String
    , gender :: Maybe Gender
    , note   :: Maybe String
    }

instance Repr Role where
    repr (Role name gender note) = intercalate "" parts
      where
        parts = ["Role(", name, ", ", genderStr, noteStr, ")"]
        genderStr = maybe "unknown" show gender
        noteStr = maybe "" (", "++) note

instance Show Role where
    show role = name role ++ "(" ++ genderStr ++ ")" ++ noteStr
      where
        genderStr = case gender role of
            Just Male ->   "m"
            Just Female -> "f"
            Nothing ->     "u"
        noteStr = maybe "" (\x -> concat ["[", x, "]"]) $ note role

-- Data type representing lines in script.
data ScriptLine = StageDirection String | Scene String | Line Role String

instance Repr ScriptLine where
    repr (StageDirection dir) = "StageDirection(" ++ dir ++ ")"
    repr (Scene scene) = "Scene(" ++ scene ++ ")"
    repr (Line role line) = "Line(" ++ show role ++ ", " ++ line ++ ")"

instance Show ScriptLine where
    show (StageDirection dir) = intercalate "" ["(", dir, ")"]
    show (Scene scene) = intercalate "" ["[", scene, "]"]
    show (Line role line) = intercalate ": " [show role, line]
