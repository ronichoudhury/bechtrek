{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Data.Bechdel.Script where
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as LT
import Data.HashMap.Lazy
import qualified Data.Text as T
import Debug.Trace

data Trek = TOS | TNG | DS9 | VOY | ENT | DSC deriving Show
$(deriveJSON defaultOptions ''Trek)

data Gender = Male | Female | Other deriving Show
$(deriveJSON defaultOptions ''Gender)

data Role = Role { name :: String, gender :: Maybe Gender } deriving Show
$(deriveJSON defaultOptions ''Role)

data Line = Dialog { role :: Role, dialog :: String, note :: Maybe String } | StageDirection String deriving Show
$(deriveJSON defaultOptions ''Line)

data Scene = Scene
  { sceneDescription :: String
  , lines :: [Line]
  } deriving Show
$(deriveJSON defaultOptions ''Scene)

data Script = Script
  { title :: String
  , series :: Trek
  , season :: Integer
  , episode :: Integer
  , scenes :: [Scene]
  } deriving Show
$(deriveJSON defaultOptions ''Script)

dumps :: Script -> LT.ByteString
dumps script = encodePretty' config script
  where
    config = let spaces = Spaces 2
                 order = (keyOrder ["role", "name", "gender", "note", "sceneDescription", "series", "title", "season", "episode"])
                 format = confNumFormat defConfig
                 trailing = confTrailingNewline defConfig
             in Config spaces order format trailing
