module Alpacc.Generator.Futhark.Lexer
  ( generateLexer )
where

import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Data.Char (ord)
import Data.Either.Extra (maybeToEither)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Tuple.Extra
import Data.Maybe (fromJust)
import Data.Foldable
import Data.FileEmbed
import Alpacc.Generator.Futhark.Util
import Data.List.Split (chunksOf)

futharkLexer :: String
futharkLexer = $(embedStringFile "futhark/lexer.fut")

tupleToStr :: (Show a, Show b) => (a, b) -> String
tupleToStr (a, b) = [i|(#{a}, #{b})|]

futharkLexerFunction :: DFA t Integer -> String
futharkLexerFunction dfa =
    [i|
def char_to_transitions (c : char) : maybe_state_vector =
  sized number_of_states <| 
  match c
  #{str_lexer_table}
  case _ -> #{str_default_case}
|]
  where
    default_case = fromJust $ defaultTransitions dfa
    table = fromJust $ parallelLexingTable dfa
    toJust = ("#just " ++)
    str_default_case = toArray $ toJust . show . snd <$> default_case
    str_lexer_table =
      futharkTableCases
        . Map.toList
        $ toArray . fmap (toJust . show . snd) <$> Map.mapKeys (show . ord) table

toBoolsLists :: Int -> [Int] -> [Bool]
toBoolsLists m ls = reverse $ map (`elem` ls) [0..m]

toBoolSet :: Int -> [Int] -> String
toBoolSet m ls = toArray . map toInt $ chunksOf 64 lists 
  where
    lists = toBoolsLists m ls
    toChar True = '1'
    toChar False = '0'
    toInt = ("0b"++) . map toChar

transitionTable :: Int -> Map ((Integer, Integer), Int) [Integer] -> String
transitionTable num_of_terminals table =
  [i|
def transition_to_terminal_set (n : (transition, char)) : terminal_bitset =
  match n
  #{cases}
  case _ -> bitset_u32.from_bit_array number_of_terminals #{empty_set}
|]
  where
    empty_set = toBoolSet num_of_terminals []
    cases = futharkTableCases . Map.toList $ _table
    toStr ((x, y), z) = [i|((#{x}, #{y}), #{z})|]
    _table =
      ([i|bitset_u32.from_bit_array number_of_terminals |]++)
      . toBoolSet num_of_terminals
      . fmap fromInteger <$> Map.mapKeys toStr table
      
findStateIntegral ::
  DFA T Integer ->
  Either String FutUInt
findStateIntegral = selectFutUInt . maximum . states

findCharIntegral ::
  DFA T Integer ->
  Either String FutUInt
findCharIntegral = selectFutUInt . toInteger . maximum . Set.map ord . alphabet

generateLexer ::
  DFA T Integer ->
  Map T Integer ->
  FutUInt ->
  Either String String
generateLexer dfa terminal_index_map terminal_type = do
  default_transitions' <- maybeToEither "The neutral transition could not be constructed." $ defaultTransitions dfa
  state_type <- findStateIntegral dfa
  char_type <- findCharIntegral dfa
  let default_transitions = toArray $ tupleToStr <$> default_transitions'
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
def dead_transitions : [number_of_states]transition = sized number_of_states #{default_transitions}
def accepting_size : i64 = #{accepting_size}
def accepting_states : [accepting_size]state = #{accepting_states_str}

type terminal_bitset = bitset_u32.bitset[(number_of_terminals - 1) / bitset_u32.nbs + 1] 
type states_bitset = bitset_u32.bitset[(number_of_states - 1) / bitset_u32.nbs + 1] 

def final_terminal_states : [number_of_terminals]states_bitset =
  sized number_of_terminals #{final_terminal_states}
def continue_terminal_states : [number_of_terminals]states_bitset =
  sized number_of_terminals #{continue_terminal_states}

def inverted_final_terminal_states : [number_of_states]terminal_bitset =
  sized number_of_states #{inverted_final_terminal_states}
def inverted_continue_terminal_states : [number_of_states]terminal_bitset =
  sized number_of_states #{inverted_continue_terminal_states}

type state_vector = [number_of_states]state
  type maybe_state_vector = [number_of_states](maybe state)

#{ignore_function}

#{lexer_function}

#{transition_function}

}

|]
  where
    number_of_terminals = length terminals
    terminals = Map.keys terminal_index_map
    lexer_function = futharkLexerFunction dfa
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
    final_terminal_states = toSetArray $ finalTerminalStates dfa
    continue_terminal_states = toSetArray $ continueTerminalStates dfa
    inverted_final_terminal_states = toInvertedSetArray . invertSetMap $ finalTerminalStates dfa
    inverted_continue_terminal_states = toInvertedSetArray . invertSetMap $ continueTerminalStates dfa
    transition_function = transitionTable number_of_terminals terminal_map
    ignore_function = 
      case T "ignore" `Map.lookup` terminal_index_map of
        Just j -> [i|def is_ignore (t : terminal) : bool = #{j} == t|]
        Nothing -> [i|def is_ignore (_ : terminal) : bool = false|]
    terminal_map = fmap (terminal_index_map Map.!) . toList <$> Map.mapKeys (second ord) (terminalMap dfa)
    accepting_states = accepting dfa
    accepting_size = show $ Set.size accepting_states
    accepting_states_str = ("sized accepting_size " ++) . toArray $ show <$> Set.toList accepting_states
    dead_state = fromJust $ unreachableState dfa