module Alpacc.HashTable
  ( hashTable
  , hashTableSize
  , HashTableMem (..)
  , UInt (..)
  )
where

import Data.Function
import System.Random.Stateful
import Control.Monad
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Composition
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Array (Array)
import Data.Array qualified as Array
import Data.List qualified as List
import Data.Either.Extra
import Data.Maybe
import Data.String.Interpolate (i)
import Alpacc.Types

data LevelOne i v =
  LevelOne
  { levelOneConsts :: [i]
  , levelOneSize :: i
  , levelOneElements :: Array i (Maybe ([i], v))
  } deriving (Eq, Ord, Show)

data HashTable i v =
  HashTable
  { levelTwoConsts :: [i]
  , levelTwoSize :: i
  , levelTwoElements :: Array i (Maybe (LevelOne i v))
  , levelTwoKeys :: Set [i]
  } deriving (Eq, Ord, Show)

data HashTableMem i v =
  HashTableMem
  { offsetArray :: Array i i
  , elementArray :: Array i (Maybe ([i], v))
  , constsArray :: Array i (Maybe [i])
  , initHashConsts :: [i]
  } deriving (Eq, Ord, Show)

hash :: IntType t => t -> Integer -> [Integer] -> [Integer] -> Integer
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

hasCollisions :: Ord b => (a -> b) -> [a] -> Bool
hasCollisions = auxiliary Set.empty
  where
    auxiliary _ _ [] = False
    auxiliary set f (x:xs) =
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
    size = (fromIntegral (Map.size table) :: Integer)^(2 :: Int)

    result consts = do
      let dead_table = Map.fromList $ (,Nothing) <$> [0..size - 1]
      let new_table =
            flip Map.union dead_table
            $ Map.mapKeys (hash int size consts)
            $ Map.mapWithKey (curry Just) table 
      let elements =
            Array.array (0, size - 1)
            $ Map.toAscList new_table
      return $
        LevelOne
        { levelOneConsts = consts
        , levelOneSize = size
        , levelOneElements = elements
        }

countLevelOne :: Integral i => LevelOne i v -> i
countLevelOne =
  sum
  . fmap (fromIntegral . fromEnum . isJust)
  . levelOneElements

hashTableMem ::
  (Show v, IntType t) =>
  t ->
  HashTable Integer v ->
  Either String (HashTableMem Integer v)
hashTableMem int hash_table
  | array_size <= intTypeMaxBound int = do
      keys <- mapM toIndex $ Set.toList $ levelTwoKeys hash_table
      return $
        HashTableMem
        { offsetArray = offset_array
        , elementArray = mkElements keys
        , constsArray = consts_array
        , initHashConsts = level_two_consts
        }
  | otherwise = hashTableMem int hash_table
  where
    level_two_size = levelTwoSize hash_table
    level_two_consts = levelTwoConsts hash_table
    level_two_elements = levelTwoElements hash_table
    countElems Nothing = 0
    countElems (Just a) = countLevelOne a
    countArraySize Nothing = 0
    countArraySize (Just a) = levelOneSize a
    offset_array =
      Array.array (0, level_two_size - 1)
      $ zip [0..]
      $ init
      $ scanl (+) 0
      $ countElems
      <$> Array.elems level_two_elements
    array_size = sum $ countArraySize <$> level_two_elements
    consts_array = fmap levelOneConsts <$> level_two_elements
    mkElements keys =
      Array.listArray (0, fromIntegral (array_size - 1))
      $ concat keys
    toIndex key = do
      level_one <-
        maybeToEither "Error: Could not create layout for hash table."
        $ level_two_elements Array.! i'
      let level_one_elements = levelOneElements level_one
      return $ Array.elems level_one_elements
      where
        i' = hash int level_two_size level_two_consts key

initHashTable' ::
  (StatefulGen g m, IntType t, Show t, Ord t) =>
  t ->
  Map [Integer] v ->
  g ->
  m (Either String (HashTable Integer v))
initHashTable' int table g
  | not is_valid = return $ Left "Error: Every key in the Map must be of the same length."
  | Just int < int' || isNothing int' = return $ Left [i|Error: #{int} is too small to create a hash table.|]
  | otherwise = do
    consts <- getConsts int consts_size g
    elements <-
      fmap (Array.array (0, size - 1)
            . Map.toAscList
            . flip Map.union dead_table
            . Map.fromList)
      $ mapM toLevelOne
      $ List.groupBy ((==) `on` fst)
      $ List.sortOn fst
      $ (\a -> (hash int size consts $ fst a,a))
      <$> Map.toList table
    return $
      Right $
      HashTable
      { levelTwoConsts = consts
      , levelTwoSize = size
      , levelTwoElements = elements
      , levelTwoKeys = Map.keysSet table
      }
  where
    ls = Map.keys table
    consts_size = if null ls then 0 else length $ head ls
    size = fromIntegral $ Map.size table
    int' = toIntType (2 * size)
    is_valid = foldl (\b a -> b && length a == consts_size) True ls
    dead_table = Map.fromList $ (,Nothing) <$> [0..size - 1]
    toLevelOne xs = do
      let table' = Map.fromList $ map snd xs
      hash_table <- initLevelOne int table' g
      return $ (k,) $ Just hash_table
      where
        (k, _) = head xs

initHashTable ::
  (IntType t, Show t, Ord t) =>
  t ->
  Int ->
  Map [Integer] v ->
  Either String (HashTable Integer v)
initHashTable int n table =
  runStateGen_ (mkStdGen n) (initHashTable' int table)

hashTableSize :: (Show t, IntType t) => Int -> Maybe t
hashTableSize = toIntType . (4*) . fromIntegral

-- | To use this function for generating a hash table you should use
-- the function hashTableSize to determine the correct size for your
-- hash table. There is some lead way in choosing a type but it might
-- take more time.
hashTable ::
  (Show v, IntType t, Show t, Ord t, IntType t) =>
  t ->
  Int ->
  Map [Integer] v ->
  Either String (HashTableMem Integer v)
hashTable int s t = do
  initHashTable int s t >>= hashTableMem int