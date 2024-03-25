{-# LANGUAGE TemplateHaskell #-}

module Alpacc.Generator.Futhark.Lexer
  ( generateLexer )
 where

import Alpacc.Generator.Futhark.Util
import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.FileEmbed
import Data.Word (Word8)
import Alpacc.Lexer.ParallelLexing
import Data.List qualified as List
import Data.Maybe
import Data.Either.Extra
import Data.Array qualified as Array

futharkLexer :: String
futharkLexer = $(embedStringFile "futhark/lexer.fut")

errorMessage :: String
errorMessage = [i|Error: Happend during Futhark code generation contact a maintainer.|]

defStateSize :: ParallelLexer Word8 Int -> String
defStateSize =
  ("def state_size : i64 = "++)
  . show
  . stateSize

defEndomorphismSize :: ParallelLexer Word8 Int -> String
defEndomorphismSize =
  ("def endomorphism_size : i64 = "++)
  . show
  . endomorphismsSize

isAcceptingArray :: ParallelLexer Word8 Int -> String
isAcceptingArray parallel_lexer =
  ([i|def is_accepting : [state_size]bool = sized state_size |]++)
  $ (++"]")
  $ ("["++)
  $ List.intercalate ", "
  $ [if j `Set.member` accepting_states then "true" else "false" | j <- [0..state_size - 1]]
  where
    state_size = stateSize parallel_lexer
    accepting_states = acceptingStates parallel_lexer

isProducingArray :: ParallelLexer Word8 Int -> String
isProducingArray parallel_lexer =
  ([i|def is_producing : [endomorphism_size]bool = sized endomorphism_size |]++)
  $ (++"]")
  $ ("["++)
  $ List.intercalate ", "
  $ [if j `Set.member` token_endos then "true" else "false" | j <- [0..endo_size - 1]]
  where
    endo_size = endomorphismsSize parallel_lexer
    token_endos = tokenEndomorphism parallel_lexer

endomorphismsToStateArray ::
  ParallelLexer Word8 Int ->
  Either String String
endomorphismsToStateArray parallel_lexer = do
  vals <-
    maybeToEither errorMessage
    $ mapM (fmap show . flip Map.lookup endos_to_states)
    [0..endomorphisms_size - 1]
  let result =
        ("def endomorphisms_to_states : [endomorphism_size]state = sized endomorphism_size "++)
        $ (++"]")
        $ ("["++)
        $ List.intercalate ",\n" vals
  return result
  where
    endomorphisms_size = endomorphismsSize parallel_lexer
    endos_to_states = endomorphismsToStates parallel_lexer

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

stateToTerminalArray :: ParallelLexer Word8 Int -> String
stateToTerminalArray parallel_lexer =
  ("def states_to_terminals : [state_size]terminal = sized state_size "++)
  $ (++"]")
  $ ("["++)
  $ List.intercalate ", "
  $ [show . fromMaybe dead_token $ Map.lookup j token_map | j <- [0..state_size - 1]]
  where
    state_size = stateSize parallel_lexer
    token_map = tokenMap parallel_lexer
    dead_token = succ $ maximum token_map

stateIntegral ::
  ParallelLexer Word8 Int ->
  Either String FutUInt
stateIntegral = selectFutUInt . toInteger . pred . stateSize

endomorphismIntegral ::
  ParallelLexer Word8 Int ->
  Either String FutUInt
endomorphismIntegral = selectFutUInt . toInteger . pred . endomorphismsSize

generateLexer ::
  ParallelDFALexer Word8 Int T ->
  Map T Int ->
  FutUInt ->
  Either String String
generateLexer lexer terminal_index_map terminal_type = do
  endomorphism_type <- endomorphismIntegral parallel_lexer
  state_type <- stateIntegral parallel_lexer
  transitions_to_endo <- transitionsToEndomorphismsArray parallel_lexer
  compositions_table <- compositionsArray parallel_lexer
  endo_to_state <- endomorphismsToStateArray parallel_lexer
  Right $
    futharkLexer
      <> [i|
module lexer = mk_lexer {
  module terminal_module = #{terminal_type}
  module state_module = #{state_type}
  module endomorphism_module = #{endomorphism_type}

  type state = state_module.t
  type endomorphism = endomorphism_module.t
  type terminal = terminal_module.t

  def identity_endomorphism : endomorphism = #{_identity}

  #{ignore_function}

  #{defEndomorphismSize parallel_lexer}

  #{defStateSize parallel_lexer}

  #{isAcceptingArray parallel_lexer}

  #{isProducingArray parallel_lexer}

  #{transitions_to_endo}

  #{compositions_table}

  #{stateToTerminalArray parallel_lexer}

  #{endo_to_state}
}
|]
  where
    terminalToIndex = (terminal_index_map Map.!)
    parallel_lexer' = parallelLexer lexer
    parallel_lexer = parallel_lexer' { tokenMap = token_map }
    token_map = terminalToIndex <$> tokenMap parallel_lexer'
    _identity = identity parallel_lexer
    ignore_function = 
      case T "ignore" `Map.lookup` terminal_index_map of
        Just j -> [i|def is_ignore (t : terminal) : bool = #{j} == t|]
        Nothing -> [i|def is_ignore (_ : terminal) : bool = false|]
