module Alpacc.Generator.C.Parser
  ( generateParser,
    generateParserWithTree,
  )
where

import Alpacc.Encode
import Alpacc.Generator.Analyzer
import Alpacc.Generator.Cuda.Cudafy
import Alpacc.HashTable
import Data.Array qualified as Array
import Data.Maybe
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text

terminalCast :: (Cudafy a) => a -> RawString
terminalCast a =
  RawString $ "(terminal_t) " <> cudafy a

generateParser :: Parser -> Text
generateParser parser =
  (Text.strip . Text.pack)
    [i|
typedef #{cudafy production_type} production_t;
typedef #{cudafy bracket_type} bracket_t;
#define Q #{cudafy q}
#define K #{cudafy k}
#define EMPTY_TERMINAL ((terminal_t) #{cudafy empty_terminal})
#define START_TERMINAL ((terminal_t) #{cudafy start_terminal})
#define END_TERMINAL ((terminal_t) #{cudafy end_terminal})
#define HASH_TABLE_SIZE #{cudafy $ length $ oaArray oa}
#define MAX_ITERS #{cudafy $ oaMaxIters oa}
#define STACKS_SIZE #{cudafy stacks_size}
#define PRODUCTIONS_SIZE #{cudafy productions_size}
#define MAX_BRACKETS_PER_POSITION #{cudafy max_brackets_per_pos}
#define MAX_PRODS_PER_POSITION #{cudafy max_prods_per_pos}
static const bracket_t STACKS[STACKS_SIZE] =
  #{cudafy stacks};
static const production_t PRODUCTIONS[PRODUCTIONS_SIZE] =
  #{cudafy productions};
static const bool HASH_TABLE_IS_VALID[HASH_TABLE_SIZE] =
  #{cudafy hash_table_is_valid};
static const terminal_t HASH_TABLE_KEYS[HASH_TABLE_SIZE][Q + K] =
  #{cudafy $ map (map terminalCast) hash_table_keys};
static const int64_t HASH_TABLE_STACKS_SPAN[HASH_TABLE_SIZE][2] =
  #{cudafy hash_table_stacks_span};
static const int64_t HASH_TABLE_PRODUCTIONS_SPAN[HASH_TABLE_SIZE][2] =
  #{cudafy hash_table_productions_span};
|]
  where
    production_type = productionType parser
    bracket_type = bracketType parser
    q = lookback parser
    k = lookahead parser
    empty_terminal = emptyTerminal parser
    start_terminal = startTerminal parser
    end_terminal = endTerminal parser
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
    max_brackets_per_pos =
      maximum $ 0 : [se - ss | ((ss, se), _) <- hash_table_spans, ss >= 0]
    max_prods_per_pos =
      maximum $ 0 : [pe - ps | (_, (ps, pe)) <- hash_table_spans, ps >= 0]

-- | Like 'generateParser' but also emits the arity and production-to-terminal
-- constants needed to build the CST parent vector in combined (both) mode.
generateParserWithTree :: Parser -> Text
generateParserWithTree parser =
  generateParser parser
    <> "\n"
    <> (Text.strip . Text.pack)
      [i|
#define ALPACC_WITH_TREE
#define NUMBER_OF_PRODUCTIONS #{cudafy number_of_productions}
static const uint64_t PRODUCTION_TO_ARITY[NUMBER_OF_PRODUCTIONS] =
  #{cudafy ari};
static const bool PRODUCTION_TO_TERMINAL_IS_VALID[NUMBER_OF_PRODUCTIONS] =
  #{cudafy production_to_terminal_is_valid};
|]
  where
    number_of_productions = numberOfProductions parser
    ari = arities parser
    production_to_terminal_is_valid = isJust <$> productionToTerminal parser
