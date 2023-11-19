module Alpacc.Generator.Futhark.Lexer
  ( generateLexer )
where

import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Tuple.Extra
import Data.Foldable
import Data.FileEmbed
import Alpacc.Generator.Futhark.Util
import Data.List.Split (chunksOf)
import Data.Word (Word8)
import Alpacc.Lexer.FSA
import Control.Monad.Identity (Identity(runIdentity))
import Data.Maybe (fromJust)

futharkLexer :: String
futharkLexer = $(embedStringFile "futhark/lexer.fut")

futharkLexerFunction :: Map Word8 [Integer] -> String
futharkLexerFunction table =
    [i|
def char_to_transitions (c : char) : state_vector =
  let vector =
  match c
  #{str_lexer_table}
  case _ -> replicate number_of_states dead_state
  in sized number_of_states vector
|]
  where
    str_lexer_table =
      futharkTableCases
        . Map.toAscList
        $ toArray . fmap show <$> Map.mapKeys show table

toBoolsLists :: Int -> [Int] -> [Bool]
toBoolsLists m ls = reverse $ map (`elem` ls) [0..m]

toBoolSet :: Int -> [Int] -> String
toBoolSet m ls = toArray . map toInt $ chunksOf 64 lists 
  where
    lists = toBoolsLists m ls
    toChar True = '1'
    toChar False = '0'
    toInt = ("0b"++) . map toChar

transitionTable :: Int -> Map ((Integer, Integer), Word8) [Integer] -> String
transitionTable num_of_terminals table =
  [i|
def transition_to_terminal_set (n : (transition, char)) : terminal_bitset =
  match n
  #{cases}
  case _ -> bitset_u32.from_bit_array number_of_terminals #{empty_set}
|]
  where
    empty_set = toBoolSet num_of_terminals []
    cases = futharkTableCases . Map.toAscList $ _table
    toStr ((x, y), z) = [i|((#{x}, #{y}), #{z})|]
    _table =
      ([i|bitset_u32.from_bit_array number_of_terminals |]++)
      . toBoolSet num_of_terminals
      . fmap fromInteger <$> Map.mapKeys toStr table
      
findStateIntegral ::
  DFA Word8 Integer ->
  Either String FutUInt
findStateIntegral = selectFutUInt . maximum . states

findCharIntegral ::
  DFA Word8 Integer ->
  Either String FutUInt
findCharIntegral = selectFutUInt . toInteger . maximum . alphabet

generateLexer ::
  DFALexer Word8 Integer T ->
  Map T Integer ->
  FutUInt ->
  Either String String
generateLexer lexer terminal_index_map terminal_type = do
  state_type <- findStateIntegral dfa
  char_type <- findCharIntegral dfa
  return $
    futharkLexer
      <> [i|
module lexer = mk_lexer {

module terminal_module = #{terminal_type}
module state_module = #{state_type}
module char_module = #{char_type}

type terminal = terminal_module.t
type state = state_module.t
type char = char_module.t

type transition = (state, state)

def number_of_states : i64 = #{number_of_states}
def number_of_terminals : i64 = #{number_of_terminals}
def initial_state : state = #{initial_state}
def dead_state : state = #{dead_state}
def accepting_size : i64 = #{accepting_size}
def accepting_states : [accepting_size]state = #{accepting_states_str}

type terminal_bitset = bitset_u32.bitset[(number_of_terminals - 1) / bitset_u32.nbs + 1] 
type states_bitset = bitset_u32.bitset[(number_of_states - 1) / bitset_u32.nbs + 1] 

def final_terminal_states : [number_of_terminals]states_bitset =
  sized number_of_terminals #{final_terminal_states}

def inverted_final_terminal_states : [number_of_states]terminal_bitset =
  sized number_of_states #{inverted_final_terminal_states}

type state_vector = [number_of_states]state
  type maybe_state_vector = [number_of_states](maybe state)

#{ignore_function}

#{lexer_function}

#{transition_function}

}

|]
  where
    dfa = fsa lexer
    dead_state = maximum $ states dfa
    table = parallelLexingTable dead_state lexer
    number_of_terminals = length terminals
    terminals = Map.keys terminal_index_map
    lexer_function = futharkLexerFunction table
    initial_state = initial dfa
    number_of_states = Set.size $ states dfa
    empty_states = Map.fromList $ (,Set.empty) <$> terminals
    inverted_empty_states = Map.fromList $ (,Set.empty) <$> toList (states dfa)
    toSetArray = 
      toArray
      . map (("bitset_u32.from_array number_of_states " ++) . show . toList . snd)
      . Map.toAscList
      . Map.mapKeys (terminal_index_map Map.!)
      . Map.unionWith Set.union empty_states
    toInvertedSetArray = 
      toArray
      . map (("bitset_u32.from_array number_of_terminals " ++) . show . toList . snd)
      . Map.toAscList
      . Map.map (Set.map (terminal_index_map Map.!))
      . Map.unionWith Set.union inverted_empty_states
    final_terminal_states = toSetArray $ finalMap lexer
    inverted_final_terminal_states = toInvertedSetArray . invertSetMap $ finalMap lexer
    transition_function = transitionTable number_of_terminals terminal_map
    ignore_function = 
      case T "ignore" `Map.lookup` terminal_index_map of
        Just j -> [i|def is_ignore (t : terminal) : bool = #{j} == t|]
        Nothing -> [i|def is_ignore (_ : terminal) : bool = false|]
    terminal_map = fmap (terminal_index_map Map.!) . toList <$> Map.mapKeys (second runIdentity) (terminalMap lexer)
    accepting_states = accepting dfa
    accepting_size = show $ Set.size accepting_states
    accepting_states_str = ("sized accepting_size " ++) . toArray $ show <$> Set.toList accepting_states
