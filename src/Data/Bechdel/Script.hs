{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Data.Bechdel.Script where
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import Data.HashMap.Lazy
import qualified Data.Text as T
import Debug.Trace

data Trek = TOS | TNG | DS9 | VOY | ENT | DSC deriving Show

{-data Dialog = Dialog-}
  {-{ role :: Role-}
  {-, dialog :: String-}
  {-} deriving Show-}

data Role = Role { name :: String } deriving Show

{-data StageDirection = StageDirection { stagedir :: String } deriving Show-}

data Line = Dialog { role :: String, dialog :: String } | StageDirection String deriving Show

data Scene = Scene
  { sceneDescription :: String
  , lines :: [Line]
  } deriving Show

data Script = Script
  { title :: String
  , series :: Trek
  , season :: Integer
  , episode :: Integer
  , scenes :: [Scene]
  } deriving Show

$(deriveJSON defaultOptions ''Role)
{-$(deriveJSON defaultOptions ''Dialog)-}
{-$(deriveJSON defaultOptions ''StageDirection)-}
{-$(deriveJSON defaultOptions ''Line)-}

instance FromJSON Line where
  parseJSON j = do
    o <- parseJSON j
    case toList (o :: Object) of
      [("dialog", Object o')] -> Dialog <$> o' .: "role" <*> o' .: "dialog"
      [("stagedir", o')] -> StageDirection <$> parseJSON o'
      _ -> fail "Line: unexpected format"

instance ToJSON Line where
  toJSON d@Dialog{} = object [
    "role" .= role d,
    "dialog" .= dialog d
    ]
  toJSON (StageDirection s) = object ["stagedir" .= T.pack s]

$(deriveJSON defaultOptions ''Scene)
$(deriveJSON defaultOptions ''Script)
$(deriveJSON defaultOptions ''Trek)
