module Alpacc.Generator.Cuda.Lexer (generateLexer) where

import Alpacc.Generator.Cuda.Cudafy
import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Alpacc.Lexer.DFAParallelLexer
import Alpacc.Lexer.Encode
import Alpacc.Lexer.ParallelLexing
import Alpacc.Types
import Data.FileEmbed
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Data.Word (Word8)

cudaLexer :: String
cudaLexer = $(embedStringFile "cuda/lexer.cu")

errorMessage :: String
errorMessage = [i|Error: Happend during Cuda code generation contact a maintainer.|]

generateLexer ::
  DFALexer Word8 Int T ->
  Map T Int ->
  IInt ->
  Either String String
generateLexer lexer terminal_index_map terminal_type = do
  int_parallel_lexer <- intDfaParallelLexer new_token_map lexer
  let ParallelLexerMasks
        { tokenMask = token_mask,
          tokenOffset = token_offset,
          indexMask = index_mask,
          indexOffset = index_offset,
          producingMask = produce_mask,
          producingOffset = produce_offset
        } = parMasks int_parallel_lexer
  let parallel_lexer = parLexer int_parallel_lexer
  let _identity = identity parallel_lexer
  let accept_array = acceptArray parallel_lexer
  endomorphism_type <- extEndoType parallel_lexer
  Right $
    [i|
using token_t = #{cudafy terminal_type};
using state_t = #{cudafy endomorphism_type};

const unsigned int NUM_STATES = #{cudafy $ endomorphismsSize parallel_lexer};
const unsigned int NUM_TRANS = 256;
#{ignore_token}
const state_t ENDO_MASK = #{cudafy index_mask};
const state_t ENDO_OFFSET = #{cudafy index_offset};
const state_t TOKEN_MASK = #{cudafy token_mask};
const state_t TOKEN_OFFSET = #{cudafy token_offset};
const state_t PRODUCE_MASK = #{cudafy produce_mask};
const state_t PRODUCE_OFFSET = #{cudafy produce_offset};
const state_t IDENTITY = #{cudafy _identity};

state_t h_to_state[NUM_TRANS] =
  #{cudafy $ Map.elems $ endomorphisms parallel_lexer};

state_t h_compose[NUM_STATES * NUM_STATES] =
  #{cudafy $ listCompositions parallel_lexer};

bool h_accept[NUM_STATES] =
  #{cudafy $ accept_array};
|]
      <> cudaLexer
  where
    defToken t = [i|#define IGNORE_TOKEN #{t}|]
    ignore_token =
      maybe "" defToken $ Map.lookup (T "ignore") terminal_index_map
    dead_token = succ $ maximum terminal_index_map
    new_token_map =
      Map.insert Nothing dead_token $
        Map.mapKeys Just terminal_index_map
