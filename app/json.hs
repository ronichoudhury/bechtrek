{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
import Control.Monad
import Data.Either
{-import Data.Maybe-}
import Data.Aeson as A
import Data.Aeson.Encode.Pretty as A
import Data.Bechdel.Script
import qualified Data.ByteString.Lazy.Char8 as LT
import Data.Text as T
import System.Exit
import NeatInterpolation

script = LT.pack . T.unpack $ [text|
  {
    "series": "TNG",
    "title": "The Ship from Neverwhere",
    "season": 4,
    "episode": 7,
    "scenes": [
      {
        "sceneDescription": "Stuff happening",
        "lines": [
          {
            "dialog": {
              "role": {
                "name": "Picard"
              },
              "dialog": "Engage!"
            }
          },
          {
            "stagedir": "Enterprise leaps to warp"
          }
        ]
      }
    ]
  }
|]

parseJson :: LT.ByteString -> Either String Script
parseJson input = A.eitherDecode input :: Either String Script

main = do
  let obj = parseJson script
  print obj

  when (isLeft obj) $ do
    putStrLn $ fromLeft undefined obj
    exitFailure

  let config = let spaces = Spaces 2
                   order = (A.keyOrder ["name", "gender", "sceneDescription", "series", "title", "season", "episode"])
                   format = confNumFormat A.defConfig
                   trailing = confTrailingNewline A.defConfig
               in Config spaces order format trailing

  let jsonText = A.encodePretty' config (fromRight undefined obj)
  LT.putStrLn jsonText
