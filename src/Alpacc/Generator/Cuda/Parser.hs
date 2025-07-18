module Alpacc.Generator.Cuda.Parser
  ( generateParser,
  )
where

import Alpacc.Generator.Analyzer
import Alpacc.Generator.Cuda.Cudafy
import Alpacc.Types
import Data.FileEmbed
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text

cudaParser :: Text
cudaParser = $(embedStringFile "cuda/parser.cu")

generateParser :: UInt -> Parser -> Text
generateParser terminal_type parser =
  (Text.strip . Text.pack)
    [i|
      #{cudafy terminal_type}
      #{q}
      #{k}
|]
    <> cudaParser
  where
    q = lookback parser
    k = lookahead parser
