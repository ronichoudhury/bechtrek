{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
import Control.Monad
import Data.Maybe
import Data.Aeson as A
import Data.Aeson.Encode.Pretty as A
import Data.Bechdel.Script
import qualified Data.ByteString.Lazy.Char8 as LT

script = "{\"series\": \"TNG\", \"title\": \"Title\", \"season\": 4, \"episode\": 7, \"scenes\": [{\"sceneDescription\": \"Stuff happening\", \"lines\": [{\"dialog\": {\"role\": \"Picard\", \"dialog\": \"Engage!\"}}, {\"stagedir\": \"Enterprise leaps to warp\"}]}]}"

parseJson input = fromJust (A.decode input :: Maybe Script)

main = do
  let obj = parseJson script
  print obj

  let config = let spaces = Spaces 2
                   order = (A.keyOrder ["sceneDescription", "series", "title", "season", "episode"])
                   format = confNumFormat A.defConfig
                   trailing = confTrailingNewline A.defConfig
               in Config spaces order format trailing

  let jsonText = A.encodePretty' config obj
  LT.putStrLn jsonText
