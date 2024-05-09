{-# LANGUAGE TemplateHaskell #-}

module Alpacc.Generator.Futhark.Lexer
  ( generateLexer )
 where

import Alpacc.Generator.Futhark.Util
import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Data.Map (Map)
import Data.Map qualified as Map
-- import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.FileEmbed
import Data.Word (Word8)
import Alpacc.Lexer.ParallelLexing
import Data.List qualified as List
import Data.Either.Extra

futharkLexer :: String
futharkLexer = $(embedStringFile "futhark/lexer.fut")

errorMessage :: String
errorMessage = [i|Error: Happend during Futhark code generation contact a maintainer.|]

defEndomorphismSize :: ParallelLexer Word8 Int -> String
defEndomorphismSize =
  ("def endomorphism_size: i64 = "++)
  . show
  . endomorphismsSize

-- isProducingArray :: ParallelLexer Word8 Int -> String
-- isProducingArray parallel_lexer =
--   ("def is_producing: [256][endomorphism_size]bool = "++)
--   $ (++"] :> [256][endomorphism_size]bool")
--   $ ("["++)
--   $ List.intercalate ",\n"
--   $ map row [0..255]
--   where
--     token_set = producesTokenSet parallel_lexer
--     state_size = endomorphismsSize parallel_lexer
--     lookup' c s =
--       if (s, c) `Set.member` token_set
--       then "true"
--       else "false"
--     row j =
--       (++"]")
--       $ ("["++)
--       $ List.intercalate ", "
--       $ map (lookup' j) [0..state_size - 1]

transitionsToEndomorphismsArray :: ParallelLexer Word8 Int -> Either String String
transitionsToEndomorphismsArray parallel_lexer = do
  vals <-
    maybeToEither errorMessage
    $ mapM (fmap show . flip Map.lookup to_endo)
    [0..255]
  let result =
        ("def transitions_to_endomorphisms : [256]endomorphism = sized 256 "++)
        $ (++"]")
        $ ("["++)
        $ List.intercalate ",\n" vals
  return result
  where
    to_endo = endomorphisms parallel_lexer
    
compositionsArray :: ParallelLexer Word8 Int -> Either String String
compositionsArray parallel_lexer = do
  vals <-
    maybeToEither errorMessage
    $ mapM row [0..endomorphisms_size - 1]
  let result =
        ("def compositions : [endomorphism_size][endomorphism_size]endomorphism = "++)
        $ (++"] :> [endomorphism_size][endomorphism_size]endomorphism")
        $ ("["++)
        $ List.intercalate ",\n" vals
  return result
  where
    _compositions = compositions parallel_lexer
    endomorphisms_size = endomorphismsSize parallel_lexer
    row j = do
      vals <-
        mapM (\k -> show <$> Map.lookup (k, j) _compositions)
        [0..endomorphisms_size - 1]
      let result =
            (++"]")
            $ ("["++)
            $ List.intercalate ", " vals
      return result

endomorphismIntegral ::
  IntParallelLexer t ->
  Either String FutUInt
endomorphismIntegral = selectFutUInt . toInteger . pred . endoSize

ignoreFunction :: Map T Int -> String
ignoreFunction terminal_index_map = 
  case T "ignore" `Map.lookup` terminal_index_map of
    Just j -> [i|def is_ignore (t : terminal) : bool = #{j} == t|]
    Nothing -> [i|def is_ignore (_ : terminal) : bool = false|]

generateLexer ::
  ParallelDFALexer Word8 Int T ->
  Map T Int ->
  FutUInt ->
  Either String String
generateLexer lexer terminal_index_map terminal_type = do
  int_parallel_lexer <-
    intParallelLexer new_token_map lexer
  let (token_mask, token_offset) = tokenMask int_parallel_lexer
  let (endo_mask, endo_offset) = endoMask int_parallel_lexer
  let (accept_mask, accept_offset) = acceptMask int_parallel_lexer
  let (produce_mask, produce_offset) = produceMask int_parallel_lexer
  let parallel_lexer = parLexer int_parallel_lexer
  let _identity = identity parallel_lexer
  endomorphism_type <- endomorphismIntegral int_parallel_lexer
  transitions_to_endo <- transitionsToEndomorphismsArray parallel_lexer
  compositions_table <- compositionsArray parallel_lexer
  Right $
    futharkLexer
      <> [i|
module lexer = mk_lexer {
  module terminal_module = #{terminal_type}
  module endomorphism_module = #{endomorphism_type}

  type endomorphism = endomorphism_module.t
  type terminal = terminal_module.t
  
  def identity_endomorphism: endomorphism = #{_identity}
  def dead_terminal: terminal = #{dead_token}
  def endo_mask: endomorphism = #{endo_mask}
  def endo_offset: endomorphism = #{endo_offset}
  def terminal_mask: endomorphism = #{token_mask}
  def terminal_offset: endomorphism = #{token_offset}
  def accept_mask: endomorphism = #{accept_mask}
  def accept_offset: endomorphism = #{accept_offset}
  def produce_mask: endomorphism = #{produce_mask}
  def produce_offset: endomorphism = #{produce_offset}

  #{ignoreFunction terminal_index_map}

  #{defEndomorphismSize parallel_lexer}

  #{transitions_to_endo}

  #{compositions_table}
}
|]
  where
    dead_token = succ $ maximum terminal_index_map
    new_token_map =
      Map.insert Nothing dead_token
      $ Map.mapKeys Just terminal_index_map
