module Alpacc.Generator.Cuda.Lexer (generateLexer) where

import Alpacc.Generator.Analyzer (Lexer (..))
import Alpacc.Generator.Cuda.Cudafy
import Alpacc.Lexer.Encode
  ( IntParallelLexer (..),
    ParallelLexerMasks (..),
  )
import Alpacc.Lexer.ParallelLexing
  ( ParallelLexer (..),
    listCompositions,
  )
import Data.FileEmbed
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Prelude hiding (lex)

cudaLexer :: Text
cudaLexer = $(embedStringFile "backends/cuda/lexer.cu")

generateLexer :: Lexer -> Text
generateLexer lex =
  (Text.strip . Text.pack)
    [i|
// state_t packs [state_index | terminal_id | produce_bit] into a single
// integer word.  All the endo/produce/terminal metadata is derived by
// masking + shifting: get_index(s), get_terminal(s), is_produce(s).
//
// Composition is a table lookup: d_compose[get_index(b) * NUM_STATES +
// get_index(a)] returns the state_t for (a ∘ b) — i.e. "apply a first,
// then b".  d_to_state[c] gives the state_t reached by consuming char c
// from the initial state.
//
// This mirrors the pre-fork CUDA lexer and the Futhark backend
// (mk_lexer): endos are opaque integer IDs, not bit-packed pairs.

using state_t = #{cudafy state_type};
using endo_t = state_t;  // legacy alias — cli.cu / templates still refer to endo_t

const size_t NUM_STATES     = #{cudafy $ endomorphismsSize parallel_lexer};
const size_t NUM_TRANS      = 256;
const state_t ENDO_MASK     = #{cudafy index_mask};
const state_t ENDO_OFFSET   = #{cudafy index_offset};
const state_t TERMINAL_MASK = #{cudafy token_mask};
const state_t TERMINAL_OFFSET = #{cudafy token_offset};
const state_t PRODUCE_MASK  = #{cudafy produce_mask};
const state_t PRODUCE_OFFSET = #{cudafy produce_offset};
const state_t IDENTITY      = #{cudafy iden};
#{ignore_tok}
const state_t h_to_state[NUM_TRANS] =
  #{cudafy $ Map.elems $ endomorphisms parallel_lexer};

const state_t h_compose[NUM_STATES * NUM_STATES] =
  #{cudafy $ listCompositions parallel_lexer};

const bool h_accept[NUM_STATES] =
  #{cudafy accept_list};
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
    accept_list = acceptArray parallel_lexer
    iden = identity parallel_lexer
    state_type = stateType lex

    defToken t = [i|#define IGNORE_TOKEN #{t}|]
    ignore_tok = maybe "" defToken $ ignoreToken lex
