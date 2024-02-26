module Alpacc.Generator.Futhark.Parser
  ( generateParser,
  )
where

import Control.DeepSeq
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
  Integer ->
  Int ->
  Int ->
  ([Integer], [Integer]) ->
  String
futharkParserTableKey empty_terminal q k =
  tupleToStr
  . both toTuple
  . BI.bimap backPad frontPad
  . both (map show)
  where
    backPad = lpad (show empty_terminal) q
    frontPad = rpad (show empty_terminal) k

-- | Creates a string that is a array in the Futhark language which corresponds
-- to the resulting productions list. This is used in the pattern matching.
futharkProductions :: Int -> Int -> ([Bracket Integer], [Int]) -> String
futharkProductions max_alpha_omega max_pi = ("#some " ++) . toTuple . toArr . snd' . fst'
  where
    toArr (a, b) = [a, b]
    snd' = BI.second (toTuple . rpad "empty_production" max_pi . map show)
    fst' = BI.first (toTuple . rpad "epsilon" max_alpha_omega . map auxiliary)
    auxiliary (LBracket a) = "left " ++ show a
    auxiliary (RBracket a) = "right " ++ show a

tupleToStr :: (Show a, Show b) => (a, b) -> String
tupleToStr (a, b) = [i|(#{a}, #{b})|]

-- | Creates a string that is the resulting LLP table which is done by using
-- pattern matching in Futhark.
futharkParserTable ::
  Integer ->
  Int ->
  Int ->
  Map ([Integer], [Integer]) ([Bracket Integer], [Int]) ->
  (Int, Int, String, String)
futharkParserTable empty_terminal q k table =
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
    stacks = toArray $ replicate max_alpha_omega "epsilon"
    rules = toArray $ replicate max_pi "empty_production"
    ne = toTuple [stacks, rules]
    last_case_str = [i|\n  case _ -> #none|]
    prods = fmap (futharkProductions max_alpha_omega max_pi)
    keys = Map.mapKeys (futharkParserTableKey empty_terminal q k)

toIntegerLLPTable ::
  (Ord nt, Ord t) =>
  Map (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)) Integer ->
  Map ([AugmentedTerminal t], [AugmentedTerminal t]) ([Bracket (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t))], [Int]) ->
  Map ([Integer], [Integer]) ([Bracket Integer], [Int])
toIntegerLLPTable symbol_index_map table = table'
  where
    table_index_keys = Map.mapKeys (both (fmap ((symbol_index_map Map.!) . Terminal))) table
    table' = first (fmap (fmap (symbol_index_map Map.!))) <$> table_index_keys

declarations :: String
declarations = [i|
type terminal = terminal_module.t
type production = production_module.t
type bracket = bracket_module.t

def empty_terminal : terminal = terminal_module.highest
def empty_production : production = production_module.highest
def epsilon : bracket = bracket_module.highest

def left (s : bracket) : bracket =
  bracket_module.set_bit (bracket_module.num_bits - 1) s 1

def right (s : bracket) : bracket =
  bracket_module.set_bit (bracket_module.num_bits - 1) s 0
|] 

findBracketIntegral ::
  Map (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)) Integer ->
  Either String FutUInt
findBracketIntegral index_map = findSize _max
  where
    _max = maximum index_map
    findSize max_size
      | max_size < 0 = Left "Max size may not be negative."
      | max_size < 2 ^ (8 - 1 :: Integer) - 1 = Right U8
      | max_size < 2 ^ (16 - 1 :: Integer) - 1 = Right U16
      | max_size < 2 ^ (32 - 1 :: Integer) - 1 = Right U32
      | max_size < 2 ^ (64 - 1 :: Integer) - 1 = Right U64
      | otherwise = Left "There are too many symbols to find a Futhark integral type."

findProductionIntegral ::
  [Production nt t] ->
  Either String FutUInt
findProductionIntegral ps = findSize _max
  where
    _max = toInteger $ length ps
    findSize max_size
      | max_size < 0 = Left "Max size may not be negative."
      | max_size < maxFutUInt U8 = Right U8
      | max_size < maxFutUInt U16 = Right U16
      | max_size < maxFutUInt U32 = Right U32
      | max_size < maxFutUInt U64 = Right U64
      | otherwise = Left "There are too many productions to find a Futhark integral type."

productionToTerminal ::
  (Ord nt, Ord t) =>
  Map (Symbol (AugmentedNonterminal (Either nt t)) (AugmentedTerminal t)) Integer ->
  [Production (AugmentedNonterminal (Either nt t)) (AugmentedTerminal t)] ->
  String
productionToTerminal symbol_to_index prods =
  ([i|sized number_of_productions [|]++)
  $ (++"]")
  $ List.intercalate "\n,"
  $ p
  . nonterminal <$> prods
  where
    p (AugmentedNonterminal (Right t)) =
      [i|#some #{x}|]
        where
          x = symbol_to_index Map.! Terminal (AugmentedTerminal t)
    p _ = "#none"    

productionToArity ::
  [Production (AugmentedNonterminal (Either nt t)) (AugmentedTerminal t)] ->
  Either String String
productionToArity prods =
  if 32767 < max_arity
  then Left "A production contains a right-hand side too many nonterminals"
  else Right arities_str
  where
    isNt (Nonterminal _) = 1 :: Integer
    isNt _ = 0
    arity = sum . fmap isNt . symbols
    arities = arity <$> prods
    max_arity = maximum arities
    arities_str =
      ([i|def production_to_arity: [number_of_productions]i16 = sized number_of_productions [|]++)
      $ (++"]")
      $ List.intercalate "\n,"
      $ show <$> arities
    

-- | Creates Futhark source code which contains a parallel parser that can
-- create the productions list for a input which is indexes of terminals.
generateParser ::
  (NFData t, NFData nt, Ord nt, Show nt, Show t, Ord t) =>
  Int ->
  Int ->
  Grammar (Either nt t) t ->
  Map (Symbol (AugmentedNonterminal (Either nt t)) (AugmentedTerminal t)) Integer ->
  FutUInt ->
  Either String String
generateParser q k grammar symbol_index_map terminal_type = do
  start_terminal <- maybeToEither "The left turnstile \"⊢\" terminal could not be found, you should complain to a developer." maybe_start_terminal
  end_terminal <- maybeToEither "The right turnstile \"⊣\" terminal could not be found, you should complain to a developer." maybe_end_terminal
  table <- llpParserTableWithStartsHomomorphisms q k grammar
  bracket_type <- findBracketIntegral symbol_index_map
  production_type <- findProductionIntegral $ productions grammar
  arities <- productionToArity prods 
  let integer_table = toIntegerLLPTable symbol_index_map table
  let (max_ao, max_pi, ne, futhark_table) =
        futharkParserTable (maxFutUInt terminal_type) q k integer_table
      brackets = List.intercalate "," $ zipWith (<>) (replicate max_ao "b") $ map show [(0 :: Int) ..]
      productions = List.intercalate "," $ zipWith (<>) (replicate max_pi "p") $ map show [(0 :: Int) ..]
  return $
    futharkParser
      <> [i|
module parser = mk_parser {

module terminal_module = #{terminal_type}
module production_module = #{production_type}
module bracket_module = #{bracket_type}

#{declarations}

type lookahead_type = #{lookahead_type}
type lookback_type = #{lookback_type}

def number_of_terminals: i64 = #{number_of_terminals}
def number_of_productions: i64 = #{number_of_productions} 
def q: i64 = #{q}
def k: i64 = #{k}
def max_ao: i64 = #{max_ao}
def max_pi: i64 = #{max_pi}
def start_terminal: terminal = #{start_terminal}
def end_terminal: terminal = #{end_terminal}
def production_to_terminal: [number_of_productions](opt terminal) =
  #{prods_to_ters}
#{arities}

def lookback_array_to_tuple [n] (arr: [n]terminal): lookback_type =
  #{toTupleIndexArray "arr" q}

def lookahead_array_to_tuple [n] (arr: [n]terminal): lookahead_type =
  #{toTupleIndexArray "arr" k}

def key_to_config (key: (lookback_type, lookahead_type)):
                  opt ([max_ao]bracket, [max_pi]production) =
  map_opt (\\((#{brackets}),(#{productions})) ->
    (sized max_ao [#{brackets}], sized max_pi [#{productions}])
  ) <|
  match key
  #{futhark_table}

def ne: ([max_ao]bracket, [max_pi]production) =
  let (a,b) = #{ne}
  in (sized max_ao a, sized max_pi b)
}
|]
  where
    prods = productions augmented_grammar
    number_of_productions = length prods
    prods_to_ters = productionToTerminal symbol_index_map prods
    number_of_terminals = length terminals'
    maybe_start_terminal = Map.lookup (Terminal RightTurnstile) symbol_index_map
    maybe_end_terminal = Map.lookup (Terminal LeftTurnstile) symbol_index_map
    augmented_grammar = augmentGrammar grammar
    terminals' = terminals augmented_grammar
    lookback_type = toTuple $ replicate q "terminal"
    lookahead_type = toTuple $ replicate k "terminal"
