module Alpacc.Generator.Futhark.Lexer
  ( generateLexer )
where

import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.String.Interpolate (i)
import Data.Tuple.Extra
import Data.FileEmbed
import Alpacc.Generator.Futhark.Util
import Data.Word (Word8)
import Alpacc.Lexer.ParallelLexing
import Data.List qualified as List
import Data.Maybe
import Data.Array qualified as Array

futharkLexer :: String
futharkLexer = $(embedStringFile "futhark/lexer.fut")

isAcceptingArray :: ParallelLexer Word8 Int -> String
isAcceptingArray parallel_lexer =
  (++"]")
  $ ("["++)
  $ List.intercalate ", "
  $ [if j `Set.member` accepting_states then "true" else "false" | j <- [0..state_size - 1]]
  where
    state_size = stateSize parallel_lexer
    accepting_states = acceptingStates parallel_lexer

endomorphismsToStateArray :: ParallelLexer Word8 Int -> String
endomorphismsToStateArray parallel_lexer =
  (++"]")
  $ ("["++)
  $ List.intercalate ", "
  $ [show . fromMaybe dead_state $ Map.lookup j endos_to_states | j <- [0..endomorphisms_size - 1]]
  where
    endomorphisms_size = endomorphismsSize parallel_lexer
    endos_to_states = endomorphismsToStates parallel_lexer
    dead_state = deadState parallel_lexer

transitionsToKeyArray :: ParallelLexer Word8 Int -> String
transitionsToKeyArray parallel_lexer =
  (++"]")
  $ ("["++)
  $ List.intercalate ", "
  $ [show . fromMaybe dead_endo $ Map.lookup j trans_to_key | j <- [0..255]]
  where
    trans_to_key = transitionsToKey parallel_lexer
    dead_endo = deadEndomorphismKey parallel_lexer

compositionsArray :: ParallelLexer Word8 Int -> String
compositionsArray parallel_lexer =
  (++"]")
  $ ("["++)
  $ List.intercalate ", "
  $ [row j | j <- [0..endomorphisms_size - 1]]
  where
    _compositions = compositions parallel_lexer
    endomorphisms_size = endomorphismsSize parallel_lexer
    dead_endo = deadEndomorphismKey parallel_lexer
    row j =
      (++"]")
      $ ("["++)
      $ List.intercalate ", "
      $ [show . fromMaybe dead_endo $ Map.lookup (k, j) _compositions | k <- [0..endomorphisms_size - 1]]

stateToTerminalArray :: ParallelLexer Word8 Int -> String
stateToTerminalArray parallel_lexer =
  (++"]")
  $ ("["++)
  $ List.intercalate ", "
  $ [show . fromMaybe dead_token $ Map.lookup j token_map | j <- [0..state_size - 1]]
  where
    state_size = stateSize parallel_lexer
    token_map = tokenMap parallel_lexer
    dead_token = maximum token_map

transitionsToEndomorphismsArray :: ParallelLexer Word8 Int -> String
transitionsToEndomorphismsArray parallel_lexer =
  (++"]")
  $ ("["++)
  $ List.intercalate ", "
  $ [fromMaybe dead_endo $ Map.lookup j trans_to_endo | j <- [0..255]]
  where
    trans_to_endo = toArrayString <$> endomorphisms parallel_lexer
    dead_endo = toArrayString $ deadEndomorphism parallel_lexer

    toArrayString :: Endomorphism -> String
    toArrayString =
      (++"]")
      . ("["++)
      . List.intercalate ", "
      . fmap show
      . Array.elems 

generateLexer ::
  DFALexer Word8 Integer T ->
  Map T Integer ->
  FutUInt ->
  Either String String
generateLexer lexer terminal_index_map' terminal_type = do
  Right $
    futharkLexer
      <> [i|
module lexer = mk_lexer {
  def dead_state : i64 = #{dead_state}
  def initial_state : i64 = #{initial_state}
  def dead_endomorphism : i64 = #{dead_endomorphism}
  def identity_endomorphism : i64 = #{_identity}

  #{isAcceptingArray parallel_lexer}

  #{transitionsToEndomorphismsArray parallel_lexer}

  #{transitionsToKeyArray parallel_lexer}

  #{compositionsArray parallel_lexer}

  #{stateToTerminalArray parallel_lexer}
}
|]
  where
    terminal_index_map = (\a -> fromIntegral a :: Int) <$> terminal_index_map'
    terminalToIndex = (terminal_index_map Map.!)
    parallel_lexer' = parallelLexer lexer
    parallel_lexer = parallel_lexer' { tokenMap = token_map }
    token_map = terminalToIndex <$> tokenMap parallel_lexer'
    dead_state = deadState parallel_lexer
    initial_state = initialState parallel_lexer
    dead_endomorphism = deadEndomorphismKey parallel_lexer
    _identity = identity parallel_lexer
    ignore_function = 
      case T "ignore" `Map.lookup` terminal_index_map of
        Just j -> [i|def is_ignore (t : i64) : bool = #{j} == t|]
        Nothing -> [i|def is_ignore (_ : i64) : bool = false|]
