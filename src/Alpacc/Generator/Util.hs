module Alpacc.Generator.Util
  ( lpad,
    rpad,
    padLLPTableKeys,
    toIntLLPTable,
    startEndIndex,
    findProductionIntType,
    findBracketIntType,
    findTerminalIntType,
    emptyTerminal,
    llpHashTable,
    padLLPTableValues,
    findAugmentedTerminalIntType,
  )
where

import Alpacc.Debug
import Alpacc.Grammar
import Alpacc.HashTable
import Alpacc.LLP
  ( Bracket (..),
    llpParserTableWithStartsHomomorphisms,
  )
import Alpacc.Types
import Control.DeepSeq
import Data.Bifunctor qualified as BI
import Data.Either.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Tuple.Extra

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
  Map (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)) Integer ->
  Map
    ( [AugmentedTerminal t],
      [AugmentedTerminal t]
    )
    ( [Bracket (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t))],
      [Int]
    ) ->
  Map ([Integer], [Integer]) ([Bracket Integer], [Int])
toIntLLPTable symbol_index_map table = table'
  where
    table_index_keys = Map.mapKeys (both (fmap (toIndex . Terminal))) table
    table' = first (fmap (fmap toIndex)) <$> table_index_keys
    toIndex = (symbol_index_map Map.!)

startEndIndex :: (Ord nt, Ord t) => Map (Symbol (AugmentedNonterminal (Either nt t)) (AugmentedTerminal t)) Integer -> Either Text (Integer, Integer)
startEndIndex symbol_index_map = do
  start_terminal <- maybeToEither "The left turnstile \"⊢\" terminal could not be found, you should complain to a developer." maybe_start_terminal
  end_terminal <- maybeToEither "The right turnstile \"⊣\" terminal could not be found, you should complain to a developer." maybe_end_terminal
  pure (start_terminal, end_terminal)
  where
    maybe_start_terminal = Map.lookup (Terminal RightTurnstile) symbol_index_map
    maybe_end_terminal = Map.lookup (Terminal LeftTurnstile) symbol_index_map

findProductionIntType ::
  Grammar nt t ->
  Either Text UInt
findProductionIntType =
  maybeToEither err
    . toIntType
    . fromIntegral
    . length
    . productions
  where
    err = "Error: There are too many productions to find a integral type."

findTerminalIntType ::
  Map t a ->
  Either Text UInt
findTerminalIntType =
  maybeToEither err
    . toIntType
    . fromIntegral
    . length
  where
    err = "Error: There are too many terminals to find a integral type."

findAugmentedTerminalIntType ::
  Grammar nt t ->
  Either Text UInt
findAugmentedTerminalIntType =
  maybeToEither err
    . toIntType
    . (+ 2)
    . fromIntegral
    . length
    . terminals
  where
    err = "Error: There are too many terminals to find a integral type."

findBracketIntType ::
  Map (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)) Integer ->
  Either Text UInt
findBracketIntType index_map
  | max_size < 0 = Left "Max size may not be negative."
  | max_size < 2 ^ (8 - 1 :: Int) - 1 = Right U8
  | max_size < 2 ^ (16 - 1 :: Int) - 1 = Right U16
  | max_size < 2 ^ (32 - 1 :: Int) - 1 = Right U32
  | max_size < 2 ^ (64 - 1 :: Int) - 1 = Right U64
  | otherwise = Left "Error: There are too many symbols to find a integral type."
  where
    max_size = toInteger $ maximum index_map

emptyTerminal :: (IntType i) => i -> Integer
emptyTerminal = intTypeMaxBound

padLLPTableValues ::
  Int ->
  Int ->
  Map ([t], [t]) ([a], [b]) ->
  Map ([t], [t]) ([Maybe a], [Maybe b])
padLLPTableValues max_ao max_pi =
  fmap (BI.bimap aoPad piPad)
  where
    aoPad = rpad Nothing max_ao . fmap Just
    piPad = rpad Nothing max_pi . fmap Just

llpHashTable ::
  (Show nt, Show t, Ord nt, Ord t, NFData nt, NFData t, IntType i, IntType t', Show i, Show t', Ord i) =>
  Int ->
  Int ->
  i ->
  t' ->
  Grammar nt t ->
  Map (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)) Integer ->
  Either Text (HashTableMem Integer (Bracket Integer) Int)
llpHashTable q k t terminal_type grammar symbol_to_index = do
  table <- llpParserTableWithStartsHomomorphisms q k grammar
  let empty_terminal = emptyTerminal terminal_type
      int_table =
        Map.mapKeys (uncurry (<>)) $
          padLLPTableKeys empty_terminal q k $
            toIntLLPTable symbol_to_index table
  tbl <- hashTable t 1 int_table
  -- mapM_ (maybeToEither "error" . hashKey t tbl) $ Map.keys int_table

  pure tbl
