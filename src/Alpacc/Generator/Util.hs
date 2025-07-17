module Alpacc.Generator.Util
  ( lpad,
    rpad,
    padLLPTableKeys,
    toIntLLPTable,
    startEndIndex,
    llpHashTable,
    padLLPTableValues,
  )
where

import Alpacc.Encode
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
  SymbolEncoder nt t ->
  Map
    ( [AugmentedTerminal t],
      [AugmentedTerminal t]
    )
    ( [Bracket (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t))],
      [Int]
    ) ->
  Map ([Integer], [Integer]) ([Bracket Integer], [Int])
toIntLLPTable encoder table = table'
  where
    table_index_keys = Map.mapKeys (both (fmap (toIndex . Terminal))) table
    table' = first (fmap (fmap toIndex)) <$> table_index_keys
    toIndex = flip symbolLookup encoder

startEndIndex :: (Ord nt, Ord t) => Map (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)) Integer -> Either Text (Integer, Integer)
startEndIndex symbol_index_map = do
  start_terminal <- maybeToEither "The left turnstile \"⊢\" terminal could not be found, you should complain to a developer." maybe_start_terminal
  end_terminal <- maybeToEither "The right turnstile \"⊣\" terminal could not be found, you should complain to a developer." maybe_end_terminal
  pure (start_terminal, end_terminal)
  where
    maybe_start_terminal = Map.lookup (Terminal RightTurnstile) symbol_index_map
    maybe_end_terminal = Map.lookup (Terminal LeftTurnstile) symbol_index_map

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
  (Show nt, Show t, Ord nt, Ord t, NFData nt, NFData t, IntType i, Show i, Ord i) =>
  Int ->
  Int ->
  i ->
  Integer ->
  Grammar (AugmentedNonterminal nt) (AugmentedTerminal t) ->
  SymbolEncoder nt t ->
  Either Text (HashTableMem Integer (Bracket Integer) Int)
llpHashTable q k t empty_terminal grammar encoder = do
  table <- llpParserTableWithStartsHomomorphisms q k grammar
  let int_table =
        Map.mapKeys (uncurry (<>)) $
          padLLPTableKeys empty_terminal q k $
            toIntLLPTable encoder table
  hashTable t 1 int_table
