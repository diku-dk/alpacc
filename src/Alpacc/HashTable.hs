module Alpacc.HashTable
  ( initHashTable
  )
where

import Data.Function
import System.Random.Stateful
import Control.Monad
import Data.Array
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word

data HashTable v =
  HashTable
  { levelOneConsts :: [Word32]
  , hashTableSize :: Word32
  , offsetArray :: Array Word32 (Maybe Word32)
  , levelTwoConsts :: Array Word32 [Word32]
  , elementArray :: Array Word32 (Maybe ([Word32], v))
  } deriving (Eq, Ord, Show)
  

hash :: Integral a => a -> [a] -> [a] -> Either String a
hash size a b =
  if ((==) `on` length) a b
  then Right $ (`mod` size) $ sum $ zipWith (*) a b
  else Left "Error: The arrays must be of equal length."

getConsts :: StatefulGen g m => Int -> g -> m [Word32]
getConsts n g = replicateM n (uniformWord32 g)

initHashTable ::
  StatefulGen g m =>
  Map [Word32] v ->
  g ->
  m (Either String [Word32])
initHashTable table g =
  if is_valid
  then return $ Left "Error: Every key in the Map must be of the same length."
  else do
    level_one_consts <- getConsts _size g
    let _indices = mapM (hash size level_one_consts) ls
    return $ Right level_one_consts
  where
    ls = Map.keys table
    _size = if null ls then 0 else length $ head ls
    size = fromIntegral _size :: Word32
    is_valid = foldl (\b a -> b && length a == _size) True ls
