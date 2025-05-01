module Alpacc.HashTable
  ( hashTable,
    hashTableSize,
    hash,
    HashTableMem (..),
    UInt (..),
  )
where

import Alpacc.Types
import Control.Monad
import Data.Array (Array)
import Data.Array qualified as Array
import Data.Composition
import Data.Function
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Random.Stateful

data LevelOne i v = LevelOne
  { levelOneConsts :: [i],
    levelOneSize :: i,
    levelOneElements :: Array i (Maybe ([i], v))
  }
  deriving (Eq, Ord, Show)

data HashTable i v = HashTable
  { levelTwoConsts :: [i],
    levelTwoSize :: i,
    levelTwoElements :: Array i (Maybe (LevelOne i v)),
    levelTwoKeys :: Set [i]
  }
  deriving (Eq, Ord, Show)

data HashTableMem i v = HashTableMem
  { offsetArray :: Array i i,
    elementArray :: Array i (Maybe ([i], v)),
    constsArray :: Array i (Maybe [i]),
    sizeArray :: Array i i,
    initHashConsts :: [i]
  }
  deriving (Eq, Ord, Show)

hash :: (IntType t) => t -> Integer -> [Integer] -> [Integer] -> Integer
hash t size = (`modAux` size) . sumIntType .: zipWith (mulIntType t)
  where
    sumIntType = List.foldl' (addIntType t) 0
    modAux = modIntType t

getConsts ::
  (StatefulGen g m, IntType t) =>
  t ->
  Int ->
  g ->
  m [Integer]
getConsts t n =
  replicateM n
    . uniformRM (intTypeMinBound t, intTypeMaxBound t)

hasCollisions :: (Ord b) => (a -> b) -> [a] -> Bool
hasCollisions = auxiliary Set.empty
  where
    auxiliary _ _ [] = False
    auxiliary set f (x : xs) =
      y `Set.member` set || auxiliary set' f xs
      where
        y = f x
        set' = Set.insert y set

initLevelOne ::
  (StatefulGen g m, IntType t) =>
  t ->
  Map [Integer] v ->
  g ->
  m (LevelOne Integer v)
initLevelOne int table g = do
  consts <- getConsts int consts_size g
  if hasCollisions (hash int size consts) keys
    then initLevelOne int table g
    else result consts
  where
    keys = Map.keys table
    consts_size = if null keys then 0 else length $ head keys
    size = (fromIntegral (Map.size table) :: Integer) ^ (2 :: Int)

    result consts = do
      let dead_table = Map.fromList $ (,Nothing) <$> [0 .. size - 1]
      let new_table =
            flip Map.union dead_table $
              Map.mapKeys (hash int size consts) $
                Map.mapWithKey (curry Just) table
      let elements =
            Array.array (0, size - 1) $
              Map.toAscList new_table
      return $
        LevelOne
          { levelOneConsts = consts,
            levelOneSize = size,
            levelOneElements = elements
          }

hashTableMem ::
  (Show v, IntType t) =>
  t ->
  HashTable Integer v ->
  Either Text (HashTableMem Integer v)
hashTableMem int hash_table
  | array_size <= intTypeMaxBound int = do
      return $
        HashTableMem
          { offsetArray = offset_array,
            elementArray = elements_array,
            constsArray = consts_array,
            initHashConsts = level_two_consts,
            sizeArray = size_array
          }
  | otherwise = hashTableMem int hash_table
  where
    level_two_size = levelTwoSize hash_table
    level_two_consts = levelTwoConsts hash_table
    level_two_elements = levelTwoElements hash_table
    countArraySize Nothing = 0
    countArraySize (Just a) = levelOneSize a
    size_array = countArraySize <$> level_two_elements
    offset_array =
      Array.array (0, level_two_size - 1) $
        zip [0 ..] $
          init $
            scanl (+) 0 $
              Array.elems size_array
    array_size = sum size_array
    elements_array =
      Array.listArray (0, array_size - 1) $
        concat $
          mapMaybe (fmap (Array.elems . levelOneElements)) $
            Array.elems level_two_elements
    consts_array = fmap levelOneConsts <$> level_two_elements

initHashTable' ::
  (StatefulGen g m, IntType t, Show t, Show v, Ord t) =>
  t ->
  Map [Integer] v ->
  g ->
  m (Either Text (HashTable Integer v))
initHashTable' int table g
  | not is_valid = pure $ Left "Error: Every key in the Map must be of the same length."
  | Just int < int' || isNothing int' = pure $ Left $ Text.pack [i|Error: #{int} is too small to create a hash table.|]
  | otherwise = do
      consts <- getConsts int consts_size g
      elements <-
        fmap
          ( Array.array (0, size - 1)
              . Map.toAscList
              . flip Map.union dead_table
              . Map.fromList
          )
          $ mapM toLevelOne
          $ List.groupBy ((==) `on` fst)
          $ List.sortOn fst
          $ (\a -> (hash int size consts $ fst a, a))
            <$> Map.toList table
      pure $
        Right $
          HashTable
            { levelTwoConsts = consts,
              levelTwoSize = size,
              levelTwoElements = elements,
              levelTwoKeys = Map.keysSet table
            }
  where
    ls = Map.keys table
    consts_size = if null ls then 0 else length $ head ls
    size = fromIntegral $ Map.size table
    int' = toIntType (2 * size)
    is_valid = foldl (\b a -> b && length a == consts_size) True ls
    dead_table = Map.fromList $ (,Nothing) <$> [0 .. size - 1]
    toLevelOne xs = do
      let table' = Map.fromList $ map snd xs
      hash_table <- initLevelOne int table' g
      return $ (k,) $ Just hash_table
      where
        (k, _) = head xs

initHashTable ::
  (IntType t, Show t, Ord t, Show v) =>
  t ->
  Int ->
  Map [Integer] v ->
  Either Text (HashTable Integer v)
initHashTable int n table =
  runStateGen_ (mkStdGen n) (initHashTable' int table)

hashTableSize :: (Show t, IntType t) => Int -> Maybe t
hashTableSize = toIntType . (4 *) . fromIntegral

-- | To use this function for generating a hash table you should use
-- the function hashTableSize to determine the correct size for your
-- hash table. There is some lead way in choosing a type but it might
-- take more time.
hashTable ::
  (Show v, IntType t, Show t, Ord t, IntType t) =>
  t ->
  Int ->
  Map [Integer] v ->
  Either Text (HashTableMem Integer v)
hashTable int s t = do
  initHashTable int s t >>= hashTableMem int
