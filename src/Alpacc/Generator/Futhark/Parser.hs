module Alpacc.Generator.Futhark.Parser
  ( generateParser,
  )
where

import Alpacc.Grammar
import Alpacc.LLP
  ( Bracket (..),
    llpParserTableWithStartsHomomorphisms,
  )
import Data.Bifunctor qualified as BI
import Data.Either.Extra (maybeToEither)
import Data.FileEmbed
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Data.Tuple.Extra
import Alpacc.Generator.Futhark.Util

futharkParser :: String
futharkParser = $(embedStringFile "futhark/parser.fut")

-- | Given the table keys for a LLP parser create the keys which will be used
-- in the Futhark language for pattern matching.
futharkParserTableKey ::
  Int ->
  Int ->
  ([Integer], [Integer]) ->
  String
futharkParserTableKey q k =
  tupleToStr
  . both toTuple
  . BI.bimap backPad frontPad
  . both (map show)
  where
    backPad = lpad "4294967295" q
    frontPad = rpad "4294967295" k

-- | Creates a string that is a array in the Futhark language which corresponds
-- to the resulting productions list. This is used in the pattern matching.
futharkProductions :: Int -> Int -> ([Bracket Integer], [Int]) -> String
futharkProductions max_alpha_omega max_pi = ("#just " ++) . toTuple . toArr . snd' . fst'
  where
    toArr (a, b) = [a, b]
    snd' = BI.second (toTuple . rpad "u32.highest" max_pi . map show)
    fst' = BI.first (toTuple . rpad "#epsilon" max_alpha_omega . map auxiliary)
    auxiliary (LBracket a) = "#left " ++ show a
    auxiliary (RBracket a) = "#right " ++ show a

tupleToStr :: (Show a, Show b) => (a, b) -> String
tupleToStr (a, b) = [i|(#{a}, #{b})|]

-- | Creates a string that is the resulting LLP table which is done by using
-- pattern matching in Futhark.
futharkParserTable ::
  Int ->
  Int ->
  Map ([Integer], [Integer]) ([Bracket Integer], [Int]) ->
  (Int, Int, String, String)
futharkParserTable q k table =
  (max_alpha_omega, max_pi, ne, )
    . (++ last_case_str)
    . cases
    . prods
    $ keys table
  where
    cases = futharkTableCases . Map.toList
    values = Map.elems table
    max_alpha_omega = maximum $ length . fst <$> values
    max_pi = maximum $ length . snd <$> values
    stacks = toArray $ replicate max_alpha_omega "#epsilon"
    rules = toArray $ replicate max_pi "u32.highest"
    ne = toTuple [stacks, rules]
    last_case_str = [i|\n  case _ -> #nothing|]
    prods = fmap (futharkProductions max_alpha_omega max_pi)
    keys = Map.mapKeys (futharkParserTableKey q k)

toIntegerLLPTable ::
  Map (Symbol (AugmentedNonterminal NT) (AugmentedTerminal T)) Integer ->
  Map ([AugmentedTerminal T], [AugmentedTerminal T]) ([Bracket (Symbol (AugmentedNonterminal NT) (AugmentedTerminal T))], [Int]) ->
  Map ([Integer], [Integer]) ([Bracket Integer], [Int])
toIntegerLLPTable symbol_index_map table = table'
  where
    table_index_keys = Map.mapKeys (both (fmap ((symbol_index_map Map.!) . Terminal))) table
    table' = first (fmap (fmap (symbol_index_map Map.!))) <$> table_index_keys

-- | Creates Futhark source code which contains a parallel parser that can
-- create the productions list for a input which is indexes of terminals.
generateParser ::
  Int ->
  Int ->
  Grammar NT T ->
  Map (Symbol (AugmentedNonterminal NT) (AugmentedTerminal T)) Integer ->
  Either String String
generateParser q k grammar symbol_index_map = do
  start_terminal <- maybeToEither "The left turnstile \"⊢\" terminal could not be found, you should complain to a developer." maybe_start_terminal
  end_terminal <- maybeToEither "The right turnstile \"⊣\" terminal could not be found, you should complain to a developer." maybe_end_terminal
  table <- llpParserTableWithStartsHomomorphisms q k grammar
  let integer_table = toIntegerLLPTable symbol_index_map table
  let (max_ao, max_pi, ne, futhark_table) =
        futharkParserTable q k integer_table
      brackets = List.intercalate "," $ zipWith (<>) (replicate max_ao "b") $ map show [(0 :: Int) ..]
      productions = List.intercalate "," $ zipWith (<>) (replicate max_pi "p") $ map show [(0 :: Int) ..]
  return $
    futharkParser
      <> [i|
module parser = mk_parser {

type lookahead_type = #{lookahead_type}
type lookback_type = #{lookback_type}

def number_of_terminals : i64 = #{number_of_terminals}
def q : i64 = #{q}
def k : i64 = #{k}
def max_ao : i64 = #{max_ao}
def max_pi : i64 = #{max_pi}
def start_terminal : terminal = #{start_terminal}
def end_terminal : terminal = #{end_terminal} 

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
}
|]
  where
    number_of_terminals = length terminals'
    maybe_start_terminal = Map.lookup (Terminal RightTurnstile) symbol_index_map
    maybe_end_terminal = Map.lookup (Terminal LeftTurnstile) symbol_index_map
    augmented_grammar = augmentGrammar grammar
    terminals' = terminals augmented_grammar
    lookback_type = toTuple $ replicate q "u32"
    lookahead_type = toTuple $ replicate k "u32"
