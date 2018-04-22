{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
import Control.Monad
import Data.Either
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty
import Data.Yaml
import qualified Data.Yaml.Pretty as P
import Data.Bechdel.Script
import qualified Data.ByteString.Char8 as T

script = "{\"series\": \"TNG\", \"title\": \"Title\", \"season\": 4, \"episode\": 7, \"scenes\": [{\"sceneDescription\": \"Stuff happening\", \"lines\": [{\"dialog\": {\"role\": \"Picard\", \"dialog\": \"Engage!\"}}, {\"stagedir\": \"Enterprise leaps to warp\"}]}]}"

yaml input = fromRight undefined $ (decodeEither input :: Either String Script)

main = do
  let config = P.setConfCompare (keyOrder ["sceneDescription", "series", "title", "season", "episode"]) P.defConfig

  putStrLn . T.unpack $ P.encodePretty config (yaml script)
