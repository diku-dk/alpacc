module Alpacc.Generator.C.Lexer
  ( generateLexer,
  )
where

import Alpacc.Generator.Analyzer
import Alpacc.Generator.Cuda.Cudafy
import Alpacc.Lexer.Encode
import Alpacc.Lexer.ParallelLexing
import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Prelude hiding (lex)

generateLexer :: Lexer -> Text
generateLexer lex =
  (Text.strip . Text.pack)
    [i|
typedef #{cudafy state_type} state_t;

#define NUM_STATES #{cudafy $ endomorphismsSize parallel_lexer}
#define NUM_TRANS 256
#{ignore_token}
#define ENDO_MASK   ((state_t) #{cudafy index_mask})
#define ENDO_OFFSET ((state_t) #{cudafy index_offset})
#define TERMINAL_MASK   ((state_t) #{cudafy token_mask})
#define TERMINAL_OFFSET ((state_t) #{cudafy token_offset})
#define PRODUCE_MASK   ((state_t) #{cudafy produce_mask})
#define PRODUCE_OFFSET ((state_t) #{cudafy produce_offset})
#define IDENTITY ((state_t) #{cudafy iden})

static const state_t TO_STATE[NUM_TRANS] =
  #{cudafy $ Map.elems $ endomorphisms parallel_lexer};

static const state_t COMPOSE[NUM_STATES * NUM_STATES] =
  #{cudafy $ listCompositions parallel_lexer};

static const bool ACCEPT[NUM_STATES] =
  #{cudafy accept_array};
|]
  where
    int_parallel_lexer = lexer lex
    ParallelLexerMasks
      { tokenMask = token_mask,
        tokenOffset = token_offset,
        indexMask = index_mask,
        indexOffset = index_offset,
        producingMask = produce_mask,
        producingOffset = produce_offset
      } = parMasks int_parallel_lexer
    parallel_lexer = parLexer int_parallel_lexer
    accept_array = acceptArray parallel_lexer
    iden = identity parallel_lexer
    state_type = stateType lex

    defToken t = [i|#define IGNORE_TOKEN ((terminal_t) #{t})|]
    ignore_token = maybe "" defToken $ ignoreToken lex
