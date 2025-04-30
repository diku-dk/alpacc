module Alpacc.Generator.Cuda.Parser
  ( generateParser,
  )
where

import Alpacc.Generator.Futhark.Futharkify
import Alpacc.Generator.Futhark.Util
import Alpacc.Grammar
import Alpacc.HashTable
import Alpacc.LLP
  ( Bracket (..),
    llpParserTableWithStartsHomomorphisms,
  )
import Alpacc.Types
import Control.DeepSeq
import Data.Array.Base as ABase
import Data.Bifunctor qualified as BI
import Data.Composition
import Data.Either.Extra (maybeToEither)
import Data.FileEmbed
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Data.Tuple.Extra

cudaParser :: String
cudaParser = $(embedStringFile "cuda/parser.cu")

padLLPTableKeys ::
  (Ord t) =>
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
  Map
    ( [AugmentedTerminal t],
      [AugmentedTerminal t]
    )
    ( [Bracket (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t))],
      [Int]
    ) ->
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
  Map
    ( [AugmentedTerminal t],
      [AugmentedTerminal t]
    )
    ( [Bracket (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t))],
      [Int]
    ) ->
  Map [Int] ([RawString], [RawString])
padAndStringifyTable empty_terminal q k max_ao max_pi =
  Map.mapKeys (uncurry (++))
    . llpTableToStrings max_ao max_pi
    . padLLPTableKeys empty_terminal q k
    .: toIntLLPTable

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
    maybeToEither "Error: The LLP table is too large." $
      hashTableSize $
        Map.size table
  let empty_terminal = fromIntegral $ intTypeMaxBound terminal_type
  let integer_table =
        padAndStringifyTable empty_terminal q k max_ao max_pi symbol_index_map table
  hash_table <- hashTable terminal_type 13 $ Map.mapKeys (fmap fromIntegral) integer_table
  let ne = createNe max_ao max_pi
  let offsets_array_str = futharkify $ offsetArray hash_table
  let hash_table_mem_size = ABase.numElements $ elementArray hash_table
  let hash_table_str =
        futharkify $
          fmap (first NTuple)
            <$> elementArray hash_table
  let consts_array = constsArray hash_table
  let consts_array_str = futharkify $ fmap (fmap NTuple) consts_array
  let size_array = futharkify $ sizeArray hash_table
  let consts = futharkify $ NTuple $ initHashConsts hash_table
  let hash_table_size = ABase.numElements consts_array
  return . (,terminal_type) $
    cudaParser
      <> [i|
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
      futharkify $
        NTuple $
          replicate (q + k) (RawString "terminal")
