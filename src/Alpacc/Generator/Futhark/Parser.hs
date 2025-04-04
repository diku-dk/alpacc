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
import Alpacc.Generator.Futhark.Futharkify
import Alpacc.Generator.Futhark.Util
import Data.Composition
import Alpacc.HashTable
import Data.Array.Base as ABase
import Alpacc.Types

futharkParser :: String
futharkParser = $(embedStringFile "futhark/parser.fut")

padLLPTableKeys ::
  Ord t =>
  t ->
  Int ->
  Int ->
  Map ([t], [t]) a ->
  Map ([t], [t]) a
padLLPTableKeys t q k =
  Map.mapKeys (BI.bimap frontPad backPad)
  where
    frontPad = lpad t q
    backPad = rpad t k

toIntLLPTable ::
  (Ord nt, Ord t) =>
  Map (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)) Int ->
  Map ([AugmentedTerminal t]
      ,[AugmentedTerminal t])
      ([Bracket (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t))]
      ,[Int]) ->
  Map ([Int], [Int]) ([Bracket Int], [Int])
toIntLLPTable symbol_index_map table = table'
  where
    table_index_keys = Map.mapKeys (both (fmap (toIndex . Terminal))) table
    table' = first (fmap (fmap toIndex)) <$> table_index_keys
    toIndex = (symbol_index_map Map.!)

llpTableToStrings ::
  Int ->
  Int ->
  Map ([t], [t]) ([Bracket Int], [Int]) ->
  Map ([t], [t]) ([RawString], [RawString])
llpTableToStrings max_ao max_pi =
  fmap (BI.bimap f g)
  where
    auxiliary (LBracket a) = "left " ++ show a
    auxiliary (RBracket a) = "right " ++ show a
    aoPad = rpad "epsilon" max_ao
    piPad = rpad "empty_production" max_pi
    f = fmap RawString . aoPad . fmap auxiliary
    g = fmap RawString . piPad . fmap show

padAndStringifyTable ::
  (Ord nt, Ord t) =>
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Map (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)) Int ->
  Map ([AugmentedTerminal t]
      ,[AugmentedTerminal t])
      ([Bracket (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t))]
      ,[Int]) ->
  Map [Int] ([RawString], [RawString])
padAndStringifyTable empty_terminal q k max_ao max_pi =
  Map.mapKeys (uncurry (++))
  . llpTableToStrings max_ao max_pi
  . padLLPTableKeys empty_terminal q k
  .: toIntLLPTable

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
  Map (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)) Int ->
  Either String UInt
findBracketIntegral index_map
  | max_size < 0 = Left "Max size may not be negative."
  | max_size < 2 ^ (8 - 1 :: Int) - 1 = Right U8
  | max_size < 2 ^ (16 - 1 :: Int) - 1 = Right U16
  | max_size < 2 ^ (32 - 1 :: Int) - 1 = Right U32
  | max_size < 2 ^ (64 - 1 :: Int) - 1 = Right U64
  | otherwise = Left "Error: There are too many symbols to find a Futhark integral type."
  where
    max_size = toInteger $ maximum index_map 

findProductionIntegral ::
  [Production nt t] ->
  Either String UInt
findProductionIntegral =
  maybeToEither err
  . toIntType
  . fromIntegral
  . length
  where
    err = "Error: There are too many productions to find a Futhark integral type."

productionToTerminal ::
  (Ord nt, Ord t) =>
  Map (Symbol (AugmentedNonterminal (Either nt t)) (AugmentedTerminal t)) Int ->
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
    
maxAoPi :: Map k ([v], [v']) -> (Int, Int)
maxAoPi table = (max_alpha_omega, max_pi)
  where
    values = Map.elems table
    max_alpha_omega = maximum $ length . fst <$> values
    max_pi = maximum $ length . snd <$> values

createNe :: Int -> Int -> String
createNe max_alpha_omega max_pi = futharkify ne
  where
    stacks = replicate max_alpha_omega (RawString "epsilon")
    rules = replicate max_pi (RawString "empty_production")
    ne = NTuple [stacks, rules]

-- | Creates a string that is a tuple where a variable is indexed from 0 to
-- n - 1 in the Futhark language.
toTupleIndexArray :: (Show a, Num a, Enum a) => String -> a -> String
toTupleIndexArray name n =
  futharkify $ NTuple $ map (indexArray name) [0 .. n - 1]

createHashFunction :: Int -> Int -> String
createHashFunction q k = 
  [i|
def hash_no_mod #{a_arg} #{b_arg} =
  #{body}
|]
  where
    qk = q + k
    a_arg = futharkify $ NTuple $ map RawString as
    b_arg = futharkify $ NTuple $ map RawString bs
    as = ["a" ++ show j | j <- [0..(qk - 1)]]
    bs = ["b" ++ show j | j <- [0..(qk - 1)]]
    concatWith a b c = b ++ a ++ c
    body =
      if qk <= 0
      then ""
      else
        List.foldl1 (concatWith " terminal_module.+ ")
        $ zipWith (
            ("("++)
            . (++")")
            .: concatWith " terminal_module.* "
          ) as bs

-- | Creates a string that indexes an array in the Futhark language.
indexArray :: Show a => String -> a -> RawString
indexArray name = RawString . (name ++) . ("[" ++) . (++"]") . show

-- | Creates Futhark source code which contains a parallel parser that can
-- create the productions list for a input which is indexes of terminals.
generateParser ::
  (NFData t, NFData nt, Ord nt, Show nt, Show t, Ord t) =>
  Int ->
  Int ->
  Grammar (Either nt t) t ->
  Map (Symbol (AugmentedNonterminal (Either nt t)) (AugmentedTerminal t)) Int ->
  Either String (String, IInt)
generateParser q k grammar symbol_index_map = do
  start_terminal <- maybeToEither "The left turnstile \"⊢\" terminal could not be found, you should complain to a developer." maybe_start_terminal
  end_terminal <- maybeToEither "The right turnstile \"⊣\" terminal could not be found, you should complain to a developer." maybe_end_terminal
  table <- llpParserTableWithStartsHomomorphisms q k grammar
  bracket_type <- findBracketIntegral symbol_index_map
  production_type <- findProductionIntegral $ productions grammar
  arities <- productionToArity prods 
  let (max_ao, max_pi) = maxAoPi table
  terminal_type <-
    maybeToEither "Error: The LLP table is too large."
    $ hashTableSize
    $ Map.size table
  let empty_terminal = fromIntegral $ intTypeMaxBound terminal_type
  let integer_table =
        padAndStringifyTable empty_terminal q k max_ao max_pi symbol_index_map table
  hash_table <- hashTable terminal_type 13 $ Map.mapKeys (fmap fromIntegral) integer_table
  let ne = createNe max_ao max_pi
  let offsets_array_str = futharkify $ offsetArray hash_table
  let hash_table_mem_size = ABase.numElements $ elementArray hash_table
  let hash_table_str =
        futharkify
        $ fmap (first NTuple)
        <$> elementArray hash_table
  let consts_array = constsArray hash_table
  let consts_array_str = futharkify $ fmap (fmap NTuple) consts_array
  let size_array = futharkify $ sizeArray hash_table
  let consts = futharkify $ NTuple $ initHashConsts hash_table
  let hash_table_size = ABase.numElements consts_array 
  return . (,terminal_type) $
    futharkParser
          <> [i|
module parser = mk_parser {

module terminal_module = #{futharkify terminal_type}
module production_module = #{futharkify production_type}
module bracket_module = #{futharkify bracket_type}

#{declarations}

type look_type = #{look_type}

def look_eq: look_type -> look_type -> bool = (==)

def number_of_terminals: i64 = #{number_of_terminals}
def number_of_productions: i64 = #{number_of_productions} 
def q: i64 = #{q}
def k: i64 = #{k}
def hash_table_size: i64 = #{hash_table_size}
def hash_table_mem_size: i64 = #{hash_table_mem_size}
def max_ao: i64 = #{max_ao}
def max_pi: i64 = #{max_pi}
def start_terminal: terminal = #{start_terminal}
def end_terminal: terminal = #{end_terminal}
def production_to_terminal: [number_of_productions](opt terminal) =
  #{prods_to_ters}
#{arities}

#{createHashFunction q k}

def array_to_look_type [n] (arr: [n]terminal): look_type =
  #{toTupleIndexArray "arr" (q+k)}

def hash_table =
  #{hash_table_str} :> [hash_table_mem_size](opt (look_type, ([max_ao]bracket, [max_pi]production)))

def offset_array =
  #{offsets_array_str} :> [hash_table_size]i64

let size_array =
  #{size_array} :> [hash_table_size]i64

def consts_array =
  #{consts_array_str} :> [hash_table_size](opt look_type)

def consts =
  #{consts} :> look_type

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
    look_type =
      futharkify
      $ NTuple
      $ replicate (q + k) (RawString "terminal")
