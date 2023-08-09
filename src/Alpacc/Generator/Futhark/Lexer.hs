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

futharkLexer :: String
futharkLexer = $(embedStringFile "futhark/lexer.fut")

tupleToStr :: (Show a, Show b) => (a, b) -> String
tupleToStr (a, b) = [i|(#{a}, #{b})|]

futharkLexerFunction :: DFA t Integer -> String
futharkLexerFunction dfa =
    [i|
def char_to_transitions (c : char) : maybe transition_vector =
  map_maybe (sized transitions_size) <| 
  match c
  #{str_lexer_table}
  case _ -> #{str_default_case}
|]
  where
    default_case = fromJust $ defaultTransitions dfa
    table = fromJust $ parallelLexingTable dfa
    toJust = ("#just " ++)
    str_default_case = toJust . toArray $ tupleToStr <$> default_case
    str_lexer_table =
      futharkTableCases
        . Map.toList
        $ toJust . toArray . fmap tupleToStr <$> Map.mapKeys (show . ord) table

transitionTable :: Map ((Integer, Integer), Int) [Integer] -> String
transitionTable table =
  [i|
def transition_to_terminal_set (n : (transition, char)) : terminal_bitset =
  match n
  #{cases}
  case _ -> bitset_u8.from_array number_of_terminals []
|]
  where
    cases = futharkTableCases . Map.toList $ _table
    toStr ((x, y), z) = [i|((#{x}, #{y}), #{z})|]
    _table = ([i|bitset_u8.from_array number_of_terminals |]++) . show <$> Map.mapKeys toStr table
      
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

def transitions_size : i64 = #{transition_size}
type transition = (state, state)
type transition_vector = [transitions_size]transition

def number_of_states : i64 = #{number_of_states}
def number_of_terminals : i64 = #{number_of_terminals}
def initial_state : state = #{initial_state}
def dead_transitions : [transitions_size]transition = sized transitions_size #{default_transitions}
def accepting_size : i64 = #{accepting_size}
def accepting_states : [accepting_size]state = #{accepting_states_str}

type terminal_bitset = bitset_u8.bitset[(number_of_terminals - 1) / bitset_u8.nbs + 1] 
type states_bitset = bitset_u8.bitset[(number_of_states - 1) / bitset_u8.nbs + 1] 

def final_terminal_states : [number_of_terminals]states_bitset =
  sized number_of_terminals #{final_terminal_states}
def continue_terminal_states : [number_of_terminals]states_bitset =
  sized number_of_terminals #{continue_terminal_states}

#{ignore_function}

#{lexer_function}

#{transition_function}

}

|]
  where
    number_of_terminals = length terminals
    terminals = Map.keys terminal_index_map
    lexer_function = futharkLexerFunction dfa
    transition_size = Set.size $ states dfa
    initial_state = initial dfa
    number_of_states = Set.size $ states dfa
    empty_states = Map.fromList $ (,Set.empty) <$> terminals
    toSetArray = 
      toArray
      . map (("bitset_u8.from_array number_of_states " ++) . show . toList . snd)
      . Map.toAscList
      . Map.mapKeys (terminal_index_map Map.!)
      . Map.unionWith Set.union empty_states
    final_terminal_states = toSetArray $ finalTerminalStates dfa
    continue_terminal_states = toSetArray $ continueTerminalStates dfa
    transition_function = transitionTable terminal_map
    ignore_function = 
      case T "ignore" `Map.lookup` terminal_index_map of
        Just j -> [i|def is_ignore (t : terminal) : bool = #{j} == t|]
        Nothing -> [i|def is_ignore (_ : terminal) : bool = false|]
    terminal_map = fmap (terminal_index_map Map.!) . toList <$> Map.mapKeys (second ord) (terminalMap dfa)
    accepting_states = accepting dfa
    accepting_size = show $ Set.size accepting_states
    accepting_states_str = ("sized accepting_size " ++) . toArray $ show <$> Set.toList accepting_states