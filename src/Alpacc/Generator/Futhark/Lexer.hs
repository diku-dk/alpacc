module Alpacc.Generator.Futhark.Lexer
  ( generateLexer )
where

import Alpacc.Grammar
import Alpacc.RegularExpression
import Data.Char (ord)
import Data.Either.Extra (maybeToEither)
import Data.List qualified as L
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Tuple.Extra
import Data.Maybe (fromJust)
import Data.Foldable
import Data.FileEmbed

futharkLexer :: String
futharkLexer = $(embedStringFile "futhark/lexer.fut")

-- | Adds square brackets to sides of a string.
squareBrackets :: String -> String
squareBrackets = ("[" ++) . (++ "]")

-- | Creates a string that is a array in the Futhark language.
toArray :: [String] -> String
toArray = squareBrackets . List.intercalate ", "

-- | Creates a string that is a single pattern matching in the Futhark Language.
-- This pattern matching is a table pair that results in a productions list.
futharkTableCase :: String -> String -> String
futharkTableCase k v = [i|case #{k} -> #{v}|]

-- | Creates a string that does pattern matching in the Futhark language.
-- The pattern matching is use to make the LLP table.
futharkTableCases :: [(String, String)] -> String
futharkTableCases = L.intercalate "\n  " . fmap (uncurry futharkTableCase)

tupleToStr :: (Show a, Show b) => (a, b) -> String
tupleToStr (a, b) = [i|(#{a}, #{b})|]

futharkLexerFunction :: DFA t Integer -> String
futharkLexerFunction dfa =
    [i|
def char_to_transitions (c : char) : maybe ([transitions_size](i64, i64)) =
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
def transition_to_terminal_set (n : ((i64, i64), char)) : bitset_u8.bitset[(number_of_terminals - 1) / bitset_u8.nbs + 1] =
  match n
  #{cases}
  case _ -> bitset_u8.from_array number_of_terminals []
|]
  where
    cases = futharkTableCases . Map.toList $ _table
    toStr ((x, y), z) = [i|((#{x}, #{y}), #{z})|]
    _table = ([i|bitset_u8.from_array number_of_terminals |]++) . show <$> Map.mapKeys toStr table

generateLexer ::
  Grammar NT T ->
  RegEx T ->
  Either String String
generateLexer grammar regex = do
  default_transitions' <- maybeToEither "The neutral transition could not be constructed." $ defaultTransitions dfa
  let default_transitions = toArray $ tupleToStr <$> default_transitions'
  return $
    futharkLexer
      <> [i|
module lexer = mk_lexer {

def number_of_states : i64 = #{number_of_states}
def number_of_terminals : i64 = #{number_of_terminals}
def initial_state : i64 = #{initial_state}
def transitions_size : i64 = #{transition_size}
def dead_transitions : [transitions_size](i64, i64) = sized transitions_size #{default_transitions}
def accepting_size : i64 = #{accepting_size}
def accepting_states : [accepting_size]i64 = #{accepting_states_str}
def final_terminal_states : [number_of_terminals](bitset_u8.bitset[(number_of_states - 1) / bitset_u8.nbs + 1]) =
  sized number_of_terminals #{final_terminal_states}
def continue_terminal_states : [number_of_terminals](bitset_u8.bitset[(number_of_states - 1) / bitset_u8.nbs + 1]) =
  sized number_of_terminals #{continue_terminal_states}

#{ignore_function}

#{lexer_function}

#{transition_function}
}
|]
  where
    number_of_terminals = length terminals'
    dfa = addDeadStateDFA $ dfaFromRegEx 0 regex
    augmented_grammar = augmentGrammar grammar
    terminals' = terminals augmented_grammar
    lexer_function = futharkLexerFunction dfa
    transition_size = Set.size $ states dfa
    initial_state = initial dfa
    number_of_states = Set.size $ states dfa
    empty_states = Map.fromList $ (,Set.empty) <$> terminals'
    toSetArray = 
      toArray
      . map (("bitset_u8.from_array number_of_states " ++) . show . toList . snd)
      . Map.toAscList
      . Map.mapKeys (terminal_index_map Map.!)
      . Map.unionWith Set.union empty_states
      . Map.mapKeys AugmentedTerminal
    final_terminal_states = toSetArray $ finalTerminalStates dfa
    continue_terminal_states = toSetArray $ continueTerminalStates dfa
    transition_function = transitionTable terminal_map
    terminal_index_map = Map.fromList $ zip terminals' [0..]
    ignore_function = 
      case AugmentedTerminal (T "ignore") `Map.lookup` terminal_index_map of
        Just j -> [i|def is_ignore (t : i64) : bool = #{j} == t|]
        Nothing -> [i|def is_ignore (_ : i64) : bool = false|]
    terminal_map = fmap (terminal_index_map Map.!) . toList . Set.map AugmentedTerminal <$> Map.mapKeys (second ord) (terminalMap dfa)
    accepting_states = accepting dfa
    accepting_size = show $ Set.size accepting_states
    accepting_states_str = ("sized accepting_size " ++) . toArray $ show <$> Set.toList accepting_states