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
using token_t = #{cudafy terminal_type};
using production_t = #{cudafy production_type};
using bracket_t = #{cudafy bracket_type};
const size_t Q = #{q};
const size_t K = #{k};
const token_t empty_terminal = #{empty_terminal};
const size_t number_of_productions = #{number_of_productions} 
|]
    <> cudaParser
  where
    production_type = productionType parser
    bracket_type = bracketType parser
    q = lookback parser
    k = lookahead parser
    empty_terminal = emptyTerminal parser
    number_of_productions = numberOfProductions parser
