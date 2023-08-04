{-# LANGUAGE TemplateHaskell #-}

module Alpacc.Generator
  ( generateParser,
  )
where

import Alpacc.Grammar
import Alpacc.LLP
  ( Bracket (..),
    llpParserTableWithStartsHomomorphisms,
  )
import Alpacc.RegularExpression
import Control.DeepSeq
import Data.Bifunctor qualified as BI
import Data.Char (ord)
import Data.Composition
import Data.Either.Extra (maybeToEither)
import Data.FileEmbed
import Data.List qualified as L
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Tuple.Extra
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import Data.Foldable

import Debug.Trace (traceShow)
debug x = traceShow x x

maxFutUInt :: FutUInt -> Integer
maxFutUInt U8 = 255
maxFutUInt U16 = 65535
maxFutUInt U32 = 4294967295
maxFutUInt U64 = 18446744073709551615

data FutUInt = U8 | U16 | U32 | U64 deriving (Ord, Eq, Bounded, Enum)

instance Show FutUInt where
  show U8 = "u8"
  show U16 = "u16"
  show U32 = "u32"
  show U64 = "u64"

selectFutUInt :: Integer -> Maybe FutUInt
selectFutUInt max_size
  | max_size < 0 = Nothing
  | max_size <= maxFutUInt U8 = Just U8
  | max_size <= maxFutUInt U16 = Just U16
  | max_size <= maxFutUInt U32 = Just U32
  | max_size <= maxFutUInt U64 = Just U64
  | otherwise = Nothing

-- | Adds m padding to the left side of a list.
lpad :: a -> Int -> [a] -> [a]
lpad p m xs = replicate (m - length ys) p ++ ys
  where
    ys = take m xs

-- | Adds m padding to the right side of a list.
rpad :: a -> Int -> [a] -> [a]
rpad p m xs = ys ++ replicate (m - length ys) p
  where
    ys = take m xs

-- | Given the table keys for a LLP parser create the keys which will be used
-- in the Futhark language for pattern matching.
futharkParserTableKey ::
  Ord t =>
  Int ->
  Int ->
  Grammar nt t ->
  [t] ->
  [t] ->
  (String, String)
futharkParserTableKey q k grammar = both toTuple . BI.bimap backPad frontPad . both convert .: (,)
  where
    terminal_map = Map.fromList . flip zip (show <$> [(0 :: Int) ..]) $ terminals grammar
    convert = map (terminal_map Map.!)
    backPad = lpad "4294967295" q
    frontPad = rpad "4294967295" k

-- | Adds square brackets to sides of a string.
squareBrackets :: String -> String
squareBrackets = ("[" ++) . (++ "]")

-- | Creates a string that indexes an array in the Futhark language.
indexArray :: Show a => String -> a -> String
indexArray name = (name ++) . squareBrackets . show

-- | Creates a string that is a tuple in the Futhark language.
toTuple :: [String] -> String
toTuple = ('(' :) . (++ ")") . List.intercalate ", "

-- | Creates a string that is a array in the Futhark language.
toArray :: [String] -> String
toArray = squareBrackets . List.intercalate ", "

-- | Creates a string that is a array in the Futhark language which corresponds
-- to the resulting productions list. This is used in the pattern matching.
futharkProductions :: Int -> Int -> ([Bracket Int], [Int]) -> String
futharkProductions max_alpha_omega max_pi = ("#just " ++) . toTuple . toArr . snd' . fst'
  where
    toArr (a, b) = [a, b]
    snd' = BI.second (toTuple . rpad "u32.highest" max_pi . map show)
    fst' = BI.first (toTuple . rpad "#epsilon" max_alpha_omega . map auxiliary)
    auxiliary (LBracket a) = "#left " ++ show a
    auxiliary (RBracket a) = "#right " ++ show a

-- | Creates a string that is a tuple where a variable is indexed from 0 to
-- n - 1 in the Futhark language.
toTupleIndexArray :: (Show a, Num a, Enum a) => String -> a -> String
toTupleIndexArray name n = toTuple $ map (indexArray name) [0 .. n - 1]

-- | Creates a string that is a single pattern matching in the Futhark Language.
-- This pattern matching is a table pair that results in a productions list.
futharkTableCase :: String -> String -> String
futharkTableCase k v = [i|case #{k} -> #{v}|]

-- | Creates a string that does pattern matching in the Futhark language.
-- The pattern matching is use to make the LLP table.
futharkTableCases :: [(String, String)] -> String
futharkTableCases = L.intercalate "\n  " . fmap (uncurry futharkTableCase)

symbolsToInts ::
  (Ord nt, Ord t) =>
  Grammar nt t ->
  Map ([t], [t]) ([Bracket (Symbol nt t)], [Int]) ->
  Map ([t], [t]) ([Bracket Int], [Int])
symbolsToInts grammar = Map.map auxiliary
  where
    ts = Terminal <$> terminals grammar
    nts = Nonterminal <$> nonterminals grammar
    mapping = Map.fromList $ zip (ts ++ nts) [0 ..]
    auxiliary (a, b) = (fmap (mapping Map.!) <$> a, b)

tupleToStr :: (Show a, Show b) => (a, b) -> String
tupleToStr (a, b) = [i|(#{a}, #{b})|]

-- | Creates a string that is the resulting LLP table which is done by using
-- pattern matching in Futhark.
futharkParserTable ::
  (Ord nt, Ord t) =>
  Int ->
  Int ->
  Grammar nt t ->
  Map ([t], [t]) ([Bracket (Symbol nt t)], [Int]) ->
  (Int, Int, String, String)
futharkParserTable q k grammar table =
  (max_alpha_omega,max_pi,ne,)
    . (++ last_case_str)
    . cases
    . prods
    . keys
    $ symbolsToInts' table
  where
    symbolsToInts' = symbolsToInts grammar
    cases = futharkTableCases . Map.toList
    values = Map.elems table
    max_alpha_omega = maximum $ length . fst <$> values
    max_pi = maximum $ length . snd <$> values
    stacks = toArray $ replicate max_alpha_omega "#epsilon"
    rules = toArray $ replicate max_pi "u32.highest"
    ne = toTuple [stacks, rules]
    last_case_str = [i|\n  case _ -> #nothing|]
    prods = fmap (futharkProductions max_alpha_omega max_pi)
    keys = Map.mapKeys (tupleToStr . uncurry (futharkParserTableKey q k grammar))

futharkParser :: String
futharkParser = $(embedStringFile "fut/parser.fut")

selectCharSize :: DFA t Integer -> Either String FutUInt
selectCharSize dfa = maybeToEither err $ selectFutUInt max_alph
  where
    max_alph = fromIntegral . maximum $ Set.map ord $ alphabet dfa
    max_uint = maxFutUInt (maxBound :: FutUInt)
    err = [i|The number of characters must be positive and less then or equal to #{max_uint}.|]

-- | This assumes the states have been reenumerated for the DFA such that each
-- states has an integer value from 0 to n and the number of states is n + 1.
selectStateSize :: DFA t Integer -> Either String FutUInt
selectStateSize dfa = maybeToEither err $ selectFutUInt max_state
  where
    max_state = fromIntegral . maximum $ states dfa
    max_uint = maxFutUInt (maxBound :: FutUInt)
    err = [i|The number of states must be positive and less then or equal to #{max_uint}.|]

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

-- terminalStateTable :: String -> Map Integer (Set Integer) -> String
-- terminalStateTable name table =
--   [i|
-- def #{name} (n : i64) : bitset_u8.bitset[(number_of_states - 1) / bitset_u8.nbs + 1] =
--   match n
--   #{cases}
--   case _ -> bitset_u8.from_array number_of_states []
-- |]
--   where
--     cases = futharkTableCases . Map.toList $ _table
--     _table = ([i|bitset_u8.from_array number_of_states |]++) . show . toList <$> Map.mapKeys show table

-- | Creates Futhark source code which contains a parallel parser that can
-- create the productions list for a input which is indexes of terminals.
generateParser ::
  (Ord nt, Ord t, Show nt, Show t, NFData t, NFData nt) =>
  Int ->
  Int ->
  Grammar nt t ->
  RegEx t ->
  Either String String
generateParser q k grammar regex = do
  start_terminal <- maybeToEither "The left turnstile \"⊢\" terminal could not be found, you should complain to a developer." maybe_start_terminal
  end_terminal <- maybeToEither "The right turnstile \"⊣\" terminal could not be found, you should complain to a developer." maybe_end_terminal
  table <- llpParserTableWithStartsHomomorphisms q k grammar
  default_transitions' <- maybeToEither "The neutral transition could not be constructed." $ defaultTransitions dfa
  dead_state <- maybeToEither "The given DFA has no dead state." $ deadState dfa
  let dead_transition = tupleToStr (dead_state, dead_state)
  let default_transitions = toArray $ tupleToStr <$> default_transitions'
  char_type <- selectCharSize dfa
  let (max_ao, max_pi, ne, futhark_table) =
        futharkParserTable q k augmented_grammar table
      -- brackets_ty = "(" <> List.intercalate "," (replicate max_ao "bracket") <> ")"
      -- productions_ty = "(" <> List.intercalate "," (replicate max_pi "u32") <> ")"
      brackets = List.intercalate "," $ zipWith (<>) (replicate max_ao "b") $ map show [(0 :: Int) ..]
      productions = List.intercalate "," $ zipWith (<>) (replicate max_pi "p") $ map show [(0 :: Int) ..]
  return $
    futharkParser
      <> [i|
module parser = mk_parser {

type lookahead_type = #{lookahead_type}
type lookback_type = #{lookback_type}
type char = #{char_type}

def number_of_states : i64 = #{number_of_states}
def number_of_terminals : i64 = #{number_of_terminals}
def initial_state : i64 = #{initial_state}
def transitions_size : i64 = #{transition_size}
def dead_transition : (i64, i64) = #{dead_transition}
def dead_transitions : [transitions_size](i64, i64) = sized transitions_size #{default_transitions}
def q : i64 = #{q}
def k : i64 = #{k}
def max_ao : i64 = #{max_ao}
def max_pi : i64 = #{max_pi}
def accepting_size : i64 = #{accepting_size}
def accepting_states : [accepting_size]i64 = #{accepting_states_str}
def start_terminal : terminal = #{start_terminal}
def end_terminal : terminal = #{end_terminal}
def final_terminal_states : [number_of_terminals](bitset_u8.bitset[(number_of_states - 1) / bitset_u8.nbs + 1]) =
  sized number_of_terminals #{final_terminal_states}
def continue_terminal_states : [number_of_terminals](bitset_u8.bitset[(number_of_states - 1) / bitset_u8.nbs + 1]) =
  sized number_of_terminals #{continue_terminal_states}

def lookback_array_to_tuple [n] (arr : [n]u32) =
  #{toTupleIndexArray "arr" q}

def lookahead_array_to_tuple [n] (arr : [n]u32) =
  #{toTupleIndexArray "arr" k}

def key_to_config (key : (lookback_type, lookahead_type))
                : maybe ([max_ao]bracket, [max_pi]u32) =
  map_maybe (\\((#{brackets}),(#{productions})) ->
    (sized max_ao [#{brackets}], sized max_pi [#{productions}])
  ) <|
  match key
  #{futhark_table}

def ne : ([max_ao]bracket, [max_pi]u32) =
  let (a,b) = #{ne}
  in (sized max_ao a, sized max_pi b)

#{lexer_function}

#{transition_function}
}
|]
  where
    number_of_terminals = length terminals'
    dfa = addDeadStateDFA $ dfaFromRegEx 0 regex
    maybe_start_terminal = List.elemIndex RightTurnstile terminals' :: Maybe Int
    maybe_end_terminal = List.elemIndex LeftTurnstile terminals' :: Maybe Int
    augmented_grammar = augmentGrammar grammar
    terminals' = terminals augmented_grammar
    lookback_type = toTuple $ replicate q "u32"
    lookahead_type = toTuple $ replicate k "u32"
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
    terminal_map = fmap (terminal_index_map Map.!) . toList . Set.map AugmentedTerminal <$> Map.mapKeys (second ord) (terminalMap dfa)
    accepting_states = accepting dfa
    accepting_size = show $ Set.size accepting_states
    accepting_states_str = ("sized accepting_size " ++) . toArray $ show <$> Set.toList accepting_states
