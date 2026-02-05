module Alpacc.Generator.Futhark.Lexer (generateLexer) where

import Alpacc.Generator.Analyzer
import Alpacc.Generator.Futhark.Futharkify
import Alpacc.Lexer.Encode
import Alpacc.Lexer.ParallelLexing
import Alpacc.Types
import Data.FileEmbed
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word8)
import Prelude hiding (lex)

futharkLexer :: Text
futharkLexer = $(embedStringFile "futhark/lexer.fut")

compositionsArray :: (Futharkify i, Integral i) => UInt -> ParallelLexer Word8 i -> Text
compositionsArray int parallel_lexer =
  Text.pack
    [i|def compositions : [state_size * state_size]state =
  #{ps} :> [state_size * state_size]state
|]
  where
    ps = futharkify $ p <$> listCompositions parallel_lexer
    p = RawString . (<> futharkify int) . futharkify

generateLexer :: UInt -> Lexer -> Text
generateLexer terminal_type lex =
  futharkLexer
    <> (Text.strip . Text.pack)
      [i|
module lexer = mk_lexer {
  module terminal_int_module = #{futharkify terminal_type}
  module state_module = #{futharkify state_type}

  type state = state_module.t
  type terminal_int = terminal_int_module.t
  type terminal = terminal

  def number_of_terminals = number_of_terminals
  def terminal_int_to_name = terminal_int_to_name :> [number_of_terminals]terminal
  def identity_state: state = #{iden}
  def dead_terminal: terminal_int = #{dead_token}
  def ignore_terminal: opt terminal_int = #{futharkify ignore_token}
  def state_mask: state = #{index_mask}
  def state_offset: state = #{index_offset}
  def terminal_mask: state = #{token_mask}
  def terminal_offset: state = #{token_offset}
  def produce_mask: state = #{produce_mask}
  def produce_offset: state = #{produce_offset}
  
  def state_size: i64 = #{states_size}
  
  def accept_array: [state_size]bool =
    sized state_size #{futharkify accept_array}

  def transitions_to_states : [256]state =
    sized 256 #{transitions}

  #{compositions_table}
}
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
    dead_token = deadToken lex
    ignore_token = ignoreToken lex
    parallel_lexer = parLexer int_parallel_lexer
    states_size = endomorphismsSize parallel_lexer
    iden = identity parallel_lexer
    accept_array = acceptArray parallel_lexer
    state_type = stateType lex
    transitions = futharkify $ transitionToState lex
    compositions_table = compositionsArray state_type parallel_lexer
