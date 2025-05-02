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
  )
where

import Alpacc.Grammar
import Alpacc.HashTable
import Alpacc.LLP
  ( Bracket (..),
  )
import Alpacc.Types
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

startEndIndex :: (Ord nt, Ord t) => Map (Symbol (AugmentedNonterminal (Either nt t)) (AugmentedTerminal t)) Int -> Either Text (Int, Int)
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
    err = "Error: There are too many productions to find a Futhark integral type."

findBracketIntType ::
  Map (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)) Int ->
  Either Text UInt
findBracketIntType index_map
  | max_size < 0 = Left "Max size may not be negative."
  | max_size < 2 ^ (8 - 1 :: Int) - 1 = Right U8
  | max_size < 2 ^ (16 - 1 :: Int) - 1 = Right U16
  | max_size < 2 ^ (32 - 1 :: Int) - 1 = Right U32
  | max_size < 2 ^ (64 - 1 :: Int) - 1 = Right U64
  | otherwise = Left "Error: There are too many symbols to find a Futhark integral type."
  where
    max_size = toInteger $ maximum index_map

findTerminalIntType ::
  Map
    ([AugmentedTerminal t], [AugmentedTerminal t])
    ( [Bracket (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t))],
      [Int]
    ) ->
  Either Text IInt
findTerminalIntType =
  maybeToEither "Error: The LLP table is too large."
    . hashTableType
    . Map.size

emptyTerminal :: (IntType i) => i -> Int
emptyTerminal = fromIntegral . intTypeMaxBound
