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

cudaPse :: Text
cudaPse = $(embedStringFile "backends/cuda/pse.cu")

cudaParser :: Text
cudaParser = $(embedStringFile "backends/cuda/parser.cu")

terminalCast :: (Cudafy a) => a -> RawString
terminalCast a =
  RawString $ "(terminal_t) " <> cudafy a

generateParser :: Parser -> Text
generateParser parser =
  cudaPse
    <> (Text.strip . Text.pack)
    [i|
using production_t = #{cudafy production_type};
using bracket_t = #{cudafy bracket_type};
const int64_t Q = #{cudafy q};
const int64_t K = #{cudafy k};
const terminal_t EMPTY_TERMINAL = #{cudafy $ terminalCast empty_terminal};
const terminal_t START_TERMINAL = #{cudafy $ terminalCast start_terminal};
const terminal_t END_TERMINAL = #{cudafy $ terminalCast end_terminal};
const int64_t NUMBER_OF_PRODUCTIONS = #{cudafy number_of_productions};
__device__ const bool PRODUCTION_TO_TERMINAL_IS_VALID[NUMBER_OF_PRODUCTIONS] =
  #{cudafy production_to_tertminal_is_valid};
__device__ const uint8_t PRODUCTION_TO_ARITY[NUMBER_OF_PRODUCTIONS] =
  #{ari};
const int64_t HASH_TABLE_SIZE = #{cudafy $ length $ oaArray oa};
const int64_t MAX_ITERS = #{cudafy $ oaMaxIters oa};
const int64_t PRODUCTIONS_SIZE = #{cudafy productions_size};
const int64_t STACKS_SIZE = #{cudafy stacks_size};
const int64_t MAX_BRACKETS_PER_POSITION = #{cudafy max_brackets_per_pos};
const int64_t MAX_PRODS_PER_POSITION = #{cudafy max_prods_per_pos};
__device__ const bracket_t STACKS[STACKS_SIZE] =
  #{cudafy stacks};
__device__ const production_t PRODUCTIONS[PRODUCTIONS_SIZE] =
  #{cudafy productions};
using span_t = #{cudafy span_type};
struct alignas(16) HashRecord {
    terminal_t key[Q + K];
    span_t ss, se, ps, pe;
};
__device__ const HashRecord HASH_TABLE[HASH_TABLE_SIZE] =
  #{cudafy hash_records};
|]
    <> cudaParser
  where
    production_type = productionType parser
    bracket_type = bracketType parser
    q = lookback parser
    k = lookahead parser
    empty_terminal = emptyTerminal parser
    start_terminal = startTerminal parser
    end_terminal = endTerminal parser
    number_of_productions = numberOfProductions parser
    production_to_tertminal_is_valid = isJust <$> productionToTerminal parser
    ari = cudafy $ arities parser
    hash_table = llpTable parser
    stacks = llpStacks hash_table
    productions = llpProductions hash_table
    stacks_size = length $ llpStacks hash_table
    productions_size = length $ llpProductions hash_table
    oa = llpOATable hash_table
    hash_table_spans =
      [spans | (_, _, spans) <- Array.elems $ oaArray oa]
    -- Spans are offsets into STACKS/PRODUCTIONS; the all-ones value of
    -- span_t is reserved as the empty-slot sentinel (checked before the
    -- key compare in lookupSpans), so the sizes must stay strictly below.
    spans_fit_u16 = stacks_size < 65535 && productions_size < 65535
    span_type = if spans_fit_u16 then U16 else U32
    span_none :: Integer
    span_none = if spans_fit_u16 then 65535 else 4294967295
    hash_record (valid, keys, ((ss, se), (ps, pe)))
      | valid =
          RawString $
            Text.pack
              [i|{#{cudafy $ map terminalCast keys}, #{ss}, #{se}, #{ps}, #{pe}}|]
      | otherwise =
          RawString $
            Text.pack
              [i|{#{cudafy $ map (const zero_terminal) keys}, #{span_none}, #{span_none}, #{span_none}, #{span_none}}|]
      where
        zero_terminal = RawString "(terminal_t) 0"
    hash_records = map hash_record $ Array.elems $ oaArray oa
    max_brackets_per_pos =
      maximum $ 0 : [se - ss | ((ss, se), _) <- hash_table_spans, ss >= 0]
    max_prods_per_pos =
      maximum $ 0 : [pe - ps | (_, (ps, pe)) <- hash_table_spans, ps >= 0]
