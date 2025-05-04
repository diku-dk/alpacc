module Alpacc.HashTable
  ( hashTable,
    hashTableLevelOneSize,
    hashTableLevelTwoSize,
    hash,
    HashTableMem (..),
    UInt (..),
    hashTableType,
  )
where

import Alpacc.Types
import Control.Monad
import Data.Array (Array)
import Data.Array qualified as Array
import Data.Array.Base as ABase
import Data.Bifunctor
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
  { levelOneConsts :: ![i],
    levelOneSize :: !i,
    levelOneElements :: !(Array i (Maybe ([i], v)))
  }
  deriving (Eq, Ord, Show)

instance Functor (LevelOne i) where
  fmap f lvl@(LevelOne {levelOneElements = a}) =
    lvl {levelOneElements = fmap (fmap (second f)) a}

data HashTable i v = HashTable
  { levelTwoConsts :: ![i],
    levelTwoSize :: !i,
    levelTwoElements :: !(Array i (Maybe (LevelOne i v))),
    levelTwoKeys :: !(Set [i]),
    levelTwoDefault :: ![i]
  }
  deriving (Eq, Ord, Show)

instance Functor (HashTable i) where
  fmap f table@(HashTable {levelTwoElements = lvls}) =
    table {levelTwoElements = fmap (fmap (fmap f)) lvls}

data HashTableMem i v1 v2 = HashTableMem
  { levelTwoOffsets :: !(Array i i),
    keysArray :: !(Array i [i]),
    stacksArray :: !(Array i v1),
    productionsArray :: !(Array i v2),
    levelOneKeysOffsets :: !(Array i i),
    levelOneStacksOffsets :: !(Array i i),
    levelOneProductionsOffsets :: !(Array i i),
    levelOneStacksSizes :: !(Array i i),
    levelOneProductionsSizes :: !(Array i i),
    constsArray :: !(Array i [i]),
    initHashConsts :: ![i],
    sizeArray :: !(Array i i)
  }
  deriving (Eq, Ord, Show)

instance Functor (HashTableMem i a) where
  fmap f table@(HashTableMem {productionsArray = a}) =
    table {productionsArray = f <$> a}

instance Bifunctor (HashTableMem i) where
  bimap f g table@(HashTableMem {stacksArray = a, productionsArray = b}) =
    table {stacksArray = f <$> a, productionsArray = g <$> b}

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

countArraySize :: (Integral i) => Maybe (LevelOne i v) -> i
countArraySize Nothing = 0
countArraySize (Just a) = levelOneSize a

constructlevelTwoArray ::
  (Integral i, Array.Ix i) =>
  HashTable i v ->
  Array i i
constructlevelTwoArray hash_table =
  listToArray $
    init $
      scanl (+) 0 $
        Array.elems size_array
  where
    level_two_elements = levelTwoElements hash_table
    size_array = countArraySize <$> level_two_elements

constructKeysValuesArray ::
  (Integral i, Array.Ix i, Show i, Show v) =>
  HashTable i v ->
  (Array i [i], Array i v)
constructKeysValuesArray hash_table =
  bimap listToArray listToArray $
    unzip $
      concat $
        mapMaybe (fmap (catMaybes . Array.elems . levelOneElements)) $
          Array.elems level_two_elements
  where
    level_two_elements = levelTwoElements hash_table

data Layout = Compact | Spacious

constructLevelOneOffsets ::
  (Integral i, Array.Ix i, Show i) =>
  HashTable i v ->
  (v -> i) ->
  Layout ->
  Array i i
constructLevelOneOffsets hash_table g b =
  listToArray $
    init $
      scanl (+) 0 flags
  where
    level_two_elements = levelTwoElements hash_table
    flags =
      mconcat
        $ mapMaybe
          ( fmap
              ( fmap (maybe 0 (g . snd))
                  . ( case b of
                        Compact -> filter isJust
                        Spacious -> id
                    )
                  . Array.elems
                  . levelOneElements
              )
          )
        $ Array.elems level_two_elements

constructLevelOneSizes ::
  (Integral i, Array.Ix i, Show i) =>
  HashTable i [v] ->
  Array i i
constructLevelOneSizes hash_table =
  listToArray vals
  where
    level_two_elements = levelTwoElements hash_table
    vals =
      mconcat
        $ mapMaybe
          ( fmap
              ( fmap (List.genericLength . snd)
                  . catMaybes
                  . Array.elems
                  . levelOneElements
              )
          )
        $ Array.elems level_two_elements

constructConstsArray :: HashTable i v -> Array i [i]
constructConstsArray hash_table =
  maybe level_two_default levelOneConsts <$> level_two_elements
  where
    level_two_elements = levelTwoElements hash_table
    level_two_default = levelTwoDefault hash_table

splitValues ::
  (Integral i, Array.Ix i) =>
  Array i ([v1], [v2]) ->
  (Array i v1, Array i v2)
splitValues =
  bimap auxiliary auxiliary
    . unzip
    . Array.elems
  where
    auxiliary = listToArray . mconcat

listToArray :: (Array.Ix i, Integral i) => [e] -> Array i e
listToArray a = Array.listArray (0, s - 1) a
  where
    s = List.genericLength a

hashTableMem ::
  (Show v1, Show v2, IntType t) =>
  t ->
  HashTable Integer ([v1], [v2]) ->
  Either Text (HashTableMem Integer v1 v2)
hashTableMem int hash_table
  | array_size <= intTypeMaxBound int = do
      pure $
        HashTableMem
          { levelTwoOffsets = level_two_offsets,
            levelOneKeysOffsets = level_one_keys_offsets,
            levelOneStacksOffsets = level_one_stacks_offsets,
            levelOneProductionsOffsets = level_one_productions_offsets,
            levelOneStacksSizes = level_one_stacks_sizes,
            levelOneProductionsSizes = level_one_productions_sizes,
            keysArray = keys_array,
            stacksArray = stacks_array,
            productionsArray = productions_array,
            constsArray = consts_array,
            initHashConsts = level_two_consts,
            sizeArray = size_array
          }
  | otherwise = hashTableMem int hash_table
  where
    level_one_keys_offsets = constructLevelOneOffsets hash_table (const 1) Spacious
    hash_table_stacks = fmap fst hash_table
    hash_table_productions = fmap snd hash_table
    level_one_stacks_sizes = constructLevelOneSizes hash_table_stacks
    level_one_productions_sizes = constructLevelOneSizes hash_table_productions
    level_one_stacks_offsets = constructLevelOneOffsets hash_table_stacks List.genericLength Compact
    level_one_productions_offsets = constructLevelOneOffsets hash_table_productions List.genericLength Compact
    level_two_consts = levelTwoConsts hash_table
    level_two_elements = levelTwoElements hash_table
    level_two_offsets = constructlevelTwoArray hash_table
    (keys_array, values_array) = constructKeysValuesArray hash_table
    (stacks_array, productions_array) = splitValues values_array
    consts_array = constructConstsArray hash_table
    size_array = countArraySize <$> level_two_elements
    array_size = sum size_array

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
              levelTwoKeys = Map.keysSet table,
              levelTwoDefault = replicate consts_size 0
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

hashTableType :: (Show t, IntType t) => Int -> Maybe t
hashTableType = toIntType . (4 *) . fromIntegral

hashTableLevelOneSize :: HashTableMem Integer v1 v2 -> Int
hashTableLevelOneSize = numElements . levelOneKeysOffsets

hashTableLevelTwoSize :: HashTableMem Integer v1 v2 -> Int
hashTableLevelTwoSize = numElements . levelTwoOffsets

-- | To use this function for generating a hash table you should use
-- the function hashTableSize to determine the correct size for your
-- hash table. There is some lead way in choosing a type but it might
-- take more time.
hashTable ::
  (Show v1, Show v2, IntType t, Show t, Ord t, IntType t) =>
  t ->
  Int ->
  Map [Integer] ([v1], [v2]) ->
  Either Text (HashTableMem Integer v1 v2)
hashTable int s t = do
  initHashTable int s t >>= hashTableMem int
