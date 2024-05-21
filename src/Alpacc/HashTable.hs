module Alpacc.HashTable
  ( initHashTable
  , hashTableMem
  , HashTable (..)
  , LevelOne (..)
  )
where

import Data.Function
import System.Random.Stateful
import Control.Monad
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word
import Data.Composition
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Bifunctor
import Data.Array (Array, Ix)
import Data.Array qualified as Array
import Data.List qualified as List
import Data.Either.Extra
import Data.Maybe
import Alpacc.Debug

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
  } deriving (Eq, Ord, Show)

hash :: Integral a => a -> [a] -> [a] -> a
hash size = (`mod` size) . sum .: zipWith (*)

getConsts :: StatefulGen g m => Int -> g -> m [Word64]
getConsts n g = replicateM n (uniformWord64 g)

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
  StatefulGen g m =>
  Map [Word64] v ->
  g ->
  m (LevelOne Word64 v)
initLevelOne table g = do
  consts <- getConsts consts_size g
  if hasCollisions (hash size consts) keys
    then initLevelOne table g
    else result consts
  where
    keys = Map.keys table
    consts_size = if null keys then 0 else length $ head keys
    size = (^(2 :: Int)) $ fromIntegral $ Map.size table :: Word64

    result consts = do
      let dead_table = Map.fromList $ (,Nothing) <$> [0..size - 1]
      let new_table =
            flip Map.union dead_table
            $ Map.mapKeys (hash size consts)
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
  (Show v, Show i, Integral i, Ix i) =>
  HashTable i v ->
  Either String (HashTableMem i v)
hashTableMem hash_table = do
  keys <- mapM toIndex $ Set.toList $ levelTwoKeys hash_table
  return $
    HashTableMem
    { offsetArray = offset_array
    , elementArray = mkElements keys
    , constsArray = consts_array
    }
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
      Array.listArray (0, array_size - 1)
      $ concat keys
    toIndex key = do
      level_one <-
        maybeToEither "Error: Could not create layout for hash table."
        $ level_two_elements Array.! i
      let level_one_elements = levelOneElements level_one
      return $ Array.elems level_one_elements
      where
        i = hash level_two_size level_two_consts key

initHashTable' ::
  StatefulGen g m =>
  Map [Word64] v ->
  g ->
  m (Either String (HashTable Word64 v))
initHashTable' table g =
  if not is_valid
  then return $ Left "Error: Every key in the Map must be of the same length."
  else do
    consts <- getConsts consts_size g
    elements <-
      fmap (Array.array (0, size - 1)
            . Map.toAscList
            . flip Map.union dead_table
            . Map.fromList)
      $ mapM toLevelOne
      $ List.groupBy ((==) `on` fst)
      $ List.sortOn fst
      $ (\a -> (hash size consts $ fst a,a))
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
    size = fromIntegral $ Map.size table :: Word64
    is_valid = foldl (\b a -> b && length a == consts_size) True ls
    dead_table = Map.fromList $ (,Nothing) <$> [0..size - 1]
    toLevelOne xs = do
      let table' = Map.fromList $ map snd xs
      hash_table <- initLevelOne table' g
      return $ (k,) $ Just hash_table
      where
        (k, _) = head xs

initHashTable ::
  Int ->
  Map [Word64] v ->
  Either String (HashTable Word64 v)
initHashTable n table =
  runStateGen_ (mkStdGen n) (initHashTable' table)
