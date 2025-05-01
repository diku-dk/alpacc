module Alpacc.Generator.Futhark.Lexer (generateLexer) where

import Alpacc.Generator.Futhark.Futharkify
import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Alpacc.Lexer.DFAParallelLexer
import Alpacc.Lexer.Encode
import Alpacc.Lexer.ParallelLexing
import Alpacc.Types
import Data.Either.Extra
import Data.FileEmbed
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word8)

futharkLexer :: Text
futharkLexer = $(embedStringFile "futhark/lexer.fut")

errorMessage :: Text
errorMessage = Text.pack [i|Error: Happend during Futhark code generation contact a maintainer.|]

defEndomorphismSize :: ParallelLexer Word8 Int -> Text
defEndomorphismSize =
  ("def endomorphism_size: i64 = " <>)
    . futharkify
    . endomorphismsSize

transitionsToEndomorphismsArray :: ParallelLexer Word8 Int -> Either Text Text
transitionsToEndomorphismsArray parallel_lexer = do
  vals <-
    maybeToEither errorMessage $
      mapM
        (fmap futharkify . flip Map.lookup to_endo)
        [0 .. 255]
  let result =
        ("def transitions_to_endomorphisms : [256]endomorphism = sized 256 " <>) $
          (<> "]") $
            ("[" <>) $
              Text.intercalate ",\n" vals
  pure result
  where
    to_endo = endomorphisms parallel_lexer

compositionsArray :: UInt -> ParallelLexer Word8 Int -> Text
compositionsArray int parallel_lexer =
  Text.pack
    [i|def compositions : [endomorphism_size * endomorphism_size]endomorphism =
  #{ps} :> [endomorphism_size * endomorphism_size]endomorphism
|]
  where
    ps = futharkify $ p <$> listCompositions parallel_lexer
    p = RawString . (<> futharkify int) . futharkify

ignoreFunction :: Map T Int -> Text
ignoreFunction terminal_index_map =
  case T "ignore" `Map.lookup` terminal_index_map of
    Just j -> Text.pack [i|def is_ignore (t : terminal) : bool = #{j} == t|]
    Nothing -> Text.pack [i|def is_ignore (_ : terminal) : bool = false|]

generateLexer ::
  DFALexer Word8 Int T ->
  Map T Int ->
  IInt ->
  Either Text Text
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
  transitions_to_endo <- transitionsToEndomorphismsArray parallel_lexer
  let compositions_table = compositionsArray endomorphism_type parallel_lexer
  Right $
    futharkLexer
      <> (Text.strip . Text.pack)
        [i|
module lexer = mk_lexer {
  module terminal_module = #{futharkify terminal_type}
  module endomorphism_module = #{futharkify endomorphism_type}

  type endomorphism = endomorphism_module.t
  type terminal = terminal_module.t
  
  def identity_endomorphism: endomorphism = #{_identity}
  def dead_terminal: terminal = #{dead_token}
  def endo_mask: endomorphism = #{index_mask}
  def endo_offset: endomorphism = #{index_offset}
  def terminal_mask: endomorphism = #{token_mask}
  def terminal_offset: endomorphism = #{token_offset}
  def produce_mask: endomorphism = #{produce_mask}
  def produce_offset: endomorphism = #{produce_offset}

  #{ignoreFunction terminal_index_map}

  #{defEndomorphismSize parallel_lexer}
  
  def accept_array: [endomorphism_size]bool =
    sized endomorphism_size #{futharkify accept_array}

  #{transitions_to_endo}

  #{compositions_table}
}
|]
  where
    dead_token = succ $ maximum terminal_index_map
    new_token_map =
      Map.insert Nothing dead_token $
        Map.mapKeys Just terminal_index_map
