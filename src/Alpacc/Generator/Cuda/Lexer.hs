module Alpacc.Generator.Cuda.Lexer (generateLexer) where

import Alpacc.Generator.Cuda.Cudafy
import Alpacc.Generator.Generator
import Alpacc.Lexer.Encode
import Alpacc.Lexer.ParallelLexing
import Alpacc.Types
import Data.FileEmbed
import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Prelude hiding (lex)

cudaLexer :: Text
cudaLexer = $(embedStringFile "cuda/lexer.cu")

generateLexer :: UInt -> Lexer -> Text
generateLexer terminal_type lex =
  (Text.strip . Text.pack)
    [i|
using token_t = #{cudafy terminal_type};
using state_t = #{cudafy state_type};

const unsigned int NUM_STATES = #{cudafy $ endomorphismsSize parallel_lexer};
const unsigned int NUM_TRANS = 256;
#{ignore_token}
const state_t ENDO_MASK = #{cudafy index_mask};
const state_t ENDO_OFFSET = #{cudafy index_offset};
const state_t TOKEN_MASK = #{cudafy token_mask};
const state_t TOKEN_OFFSET = #{cudafy token_offset};
const state_t PRODUCE_MASK = #{cudafy produce_mask};
const state_t PRODUCE_OFFSET = #{cudafy produce_offset};
const state_t IDENTITY = #{cudafy iden};

state_t h_to_state[NUM_TRANS] =
  #{cudafy $ Map.elems $ endomorphisms parallel_lexer};

state_t h_compose[NUM_STATES * NUM_STATES] =
  #{cudafy $ listCompositions parallel_lexer};

bool h_accept[NUM_STATES] =
  #{cudafy $ accept_array};
|]
    <> cudaLexer
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

    defToken t = [i|#define IGNORE_TOKEN #{t}|]
    ignore_token =
      maybe "" defToken $ ignoreToken lex
