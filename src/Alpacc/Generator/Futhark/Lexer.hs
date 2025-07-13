module Alpacc.Generator.Futhark.Lexer (generateLexer) where

import Alpacc.Generator.Futhark.Futharkify
import Alpacc.Generator.Generator
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
    [i|def compositions : [endomorphism_size * endomorphism_size]endomorphism =
  #{ps} :> [endomorphism_size * endomorphism_size]endomorphism
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
  module terminal_module = #{futharkify terminal_type}
  module endomorphism_module = #{futharkify state_type}

  type endomorphism = endomorphism_module.t
  type terminal = terminal_module.t
  
  def identity_endomorphism: endomorphism = #{iden}
  def dead_terminal: terminal = #{dead_token}
  def endo_mask: endomorphism = #{index_mask}
  def endo_offset: endomorphism = #{index_offset}
  def terminal_mask: endomorphism = #{token_mask}
  def terminal_offset: endomorphism = #{token_offset}
  def produce_mask: endomorphism = #{produce_mask}
  def produce_offset: endomorphism = #{produce_offset}
  
  def endomorphism_size: i64 = #{endomorphisms_size}
  
  def accept_array: [endomorphism_size]bool =
    sized endomorphism_size #{futharkify accept_array}

  def transitions_to_endomorphisms : [256]endomorphism =
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
    parallel_lexer = parLexer int_parallel_lexer
    endomorphisms_size = endomorphismsSize parallel_lexer
    iden = identity parallel_lexer
    accept_array = acceptArray parallel_lexer
    state_type = stateType lex
    transitions = futharkify $ transitionToState lex
    compositions_table = compositionsArray state_type parallel_lexer
