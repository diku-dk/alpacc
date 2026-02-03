module Alpacc.Generator.Cuda.Parser
  ( generateParser,
  )
where

import Alpacc.Generator.Analyzer
import Alpacc.Generator.Cuda.Cudafy
import Alpacc.Types
import Data.FileEmbed
import Data.Maybe
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text

cudaParser :: Text
cudaParser = $(embedStringFile "cuda/parser.cu")

generateParser :: UInt -> Parser -> Text
generateParser terminal_type parser =
  (Text.strip . Text.pack)
    [i|
using terminal_t = #{cudafy terminal_type};
using production_t = #{cudafy production_type};
using bracket_t = #{cudafy bracket_type};
const size_t Q = #{cudafy q};
const size_t K = #{cudafy k};
const terminal_t EMPTY_TERMINAL = #{cudafy empty_terminal};
const size_t NUMBER_OF_PRODUCTIONS = #{cudafy number_of_productions};
const terminal_t PRODUCTION_TO_TERMINAL[NUMBER_OF_PRODUCTIONS] =
  #{cudafy production_to_tertminal};
const bool PRODUCTION_TO_TERMINAL_IS_VALID[NUMBER_OF_PRODUCTIONS] =
  #{cudafy production_to_tertminal_is_valid};
const size_t PRODUCTION_TO_ARITY =
  
|]
    <> cudaParser
  where
    production_type = productionType parser
    bracket_type = bracketType parser
    q = lookback parser
    k = lookahead parser
    empty_terminal = emptyTerminal parser
    number_of_productions = numberOfProductions parser
    production_to_tertminal = fromMaybe 0 <$> productionToTerminal parser
    production_to_tertminal_is_valid = isJust <$> productionToTerminal parser
