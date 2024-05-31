module Alpacc.HashTable
  ( hashTable
  , HashTableMem (..)
  , UInt (..)
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
import Data.Array (Array, Ix)
import Data.Array qualified as Array
import Data.List qualified as List
import Data.Either.Extra
import Data.Maybe
import Data.String.Interpolate (i)
import Alpacc.Types
import Numeric.Natural

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

hash :: Integral i => i -> [i] -> [i] -> i
hash size = (`mod` size) . sum .: zipWith (*)

hash8 :: Word8 -> [Word8] -> [Word8] -> Word8
hash8 = hash

hash16 :: Word16 -> [Word16] -> [Word16] -> Word16
hash16 = hash

hash32 :: Word32 -> [Word32] -> [Word32] -> Word32
hash32 = hash

hash64 :: Word64 -> [Word64] -> [Word64] -> Word64
hash64 = hash

hashConvert ::
  Integral i =>
  (i -> [i] -> [i] -> i) ->
  Natural ->
  [Natural] ->
  [Natural] ->
  Natural
hashConvert h s =
  fromIntegral .: h (fromIntegral s) `on` map fromIntegral

hashT :: UInt -> Natural -> [Natural] -> [Natural] -> Natural
hashT U8 = hashConvert hash8
hashT U16 = hashConvert hash16
hashT U32 = hashConvert hash32
hashT U64 = hashConvert hash64

getConsts ::
  (StatefulGen g m) =>
  UInt ->
  Int ->
  g ->
  m [Natural]
getConsts U8 n = replicateM n . fmap fromIntegral . uniformWord8
getConsts U16 n = replicateM n . fmap fromIntegral . uniformWord16
getConsts U32 n = replicateM n . fmap fromIntegral . uniformWord32
getConsts U64 n = replicateM n . fmap fromIntegral . uniformWord64

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
  UInt ->
  Map [Natural] v ->
  g ->
  m (LevelOne Natural v)
initLevelOne uint table g = do
  consts <- getConsts uint consts_size g
  if hasCollisions (hashT uint size consts) keys
    then initLevelOne uint table g
    else result consts
  where
    keys = Map.keys table
    consts_size = if null keys then 0 else length $ head keys
    size = (fromIntegral (Map.size table) :: Natural)^(2 :: Int)

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
  Show v =>
  HashTable Natural v ->
  Either String (HashTableMem Natural v)
hashTableMem hash_table
  | array_size <= 4 * fromIntegral level_two_size = do
      keys <- mapM toIndex $ Set.toList $ levelTwoKeys hash_table
      return $
        HashTableMem
        { offsetArray = offset_array
        , elementArray = mkElements keys
        , constsArray = consts_array
        , initHashConsts = level_two_consts
        }
  | otherwise = hashTableMem hash_table
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
        i' = hash level_two_size level_two_consts key

initHashTable' ::
  StatefulGen g m =>
  UInt ->
  Map [Natural] v ->
  g ->
  m (Either String (HashTable Natural v))
initHashTable' uint table g
  | not is_valid = return $ Left "Error: Every key in the Map must be of the same length."
  | Just uint < uint' && isJust uint' = return $ Left [i|Error: #{uint} is too small to create a hash table.|]
  | otherwise = do
    consts <- getConsts uint consts_size g
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
    size = fromIntegral $ Map.size table
    uint' = toUInt size
    is_valid = foldl (\b a -> b && length a == consts_size) True ls
    dead_table = Map.fromList $ (,Nothing) <$> [0..size - 1]
    toLevelOne xs = do
      let table' = Map.fromList $ map snd xs
      hash_table <- initLevelOne uint table' g
      return $ (k,) $ Just hash_table
      where
        (k, _) = head xs

initHashTable ::
  UInt ->
  Int ->
  Map [Natural] v ->
  Either String (HashTable Natural v)
initHashTable uint n table =
  runStateGen_ (mkStdGen n) (initHashTable' uint table)

hashTableSize :: Int -> Maybe UInt
hashTableSize = toUInt . (4*) . fromIntegral

hashTable ::
  Show v =>
  Int ->
  Map [Natural] v ->
  Either String (HashTableMem Natural v, UInt)
hashTable s t = do
  uint <- maybeToEither "Error: The table was too large to create a hash table from." $ hashTableSize $ Map.size t
  fmap (,uint) $ initHashTable uint s t >>= hashTableMem
