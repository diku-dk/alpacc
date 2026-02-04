module Alpacc.Generator.Cuda.Parser
  ( generateParser,
  )
where

import Alpacc.Encode
import Alpacc.Generator.Analyzer
import Alpacc.Generator.Cuda.Cudafy
import Alpacc.HashTable
import Alpacc.Types
import Data.Array qualified as Array
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
const size_t PRODUCTION_TO_ARITY[NUMBER_OF_PRODUCTIONS] =
  #{ari};
const size_t HASH_TABLE_SIZE = #{cudafy $ length $ oaArray oa};
const size_t MAX_ITERS = #{cudafy $ oaMaxIters oa};
const size_t PRODUCTIONS_SIZE = #{cudafy productions_size};
const size_t STACKS_SIZE = #{cudafy stacks_size};
const bracket_t STACKS[STACKS_SIZE] =
  #{cudafy stacks};
const size_t PRODUCTIONS[STACKS_SIZE] =
  #{cudafy productions};
const bool HASH_TABLE_IS_VALID[HASH_TABLE_SIZE] =
  #{cudafy hash_table_is_valid};
const terminal_t HASH_TABLE_KEYS[HASH_TABLE_SIZE][Q + K] =
  #{cudafy hash_table_keys};
const size_t HASH_TABLE_STACKS_SPAN[HASH_TABLE_SIZE][2] =
  #{cudafy hash_table_stacks_span};
const size_t HASH_TABLE_PRODUCTIONS_SPAN[HASH_TABLE_SIZE][2] =
  #{cudafy hash_table_productions_span};
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
    ari = cudafy $ arities parser
    hash_table = llpTable parser
    stacks = llpStacks hash_table
    productions = llpProductions hash_table
    stacks_size = length $ llpStacks hash_table
    productions_size = length $ llpProductions hash_table
    oa = llpOATable hash_table
    ( hash_table_is_valid,
      hash_table_keys,
      hash_table_spans
      ) = unzip3 $ Array.elems $ oaArray oa
    ( hash_table_stacks_span,
      hash_table_productions_span
      ) = unzip hash_table_spans
