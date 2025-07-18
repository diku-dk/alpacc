module Alpacc.HashTable
  ( OpenAddressing (..),
    openAdressing,
  )
where

import Data.Array (Array)
import Data.Array qualified as Array
import Data.Bifunctor
import Data.Map (Map)
import Data.Map qualified as Map

data OpenAddressing k v
  = OpenAddressing
  { oaArray :: Array Int (Bool, k, v),
    oaMaxIters :: Int
  }
  deriving (Show, Ord, Eq)

instance Functor (OpenAddressing k) where
  fmap f oa@(OpenAddressing {oaArray = a}) =
    oa {oaArray = fmap (second f) a}

insertOpenAdressing :: (Integral i) => i -> (i, k, v) -> OpenAddressing k v -> OpenAddressing k v
insertOpenAdressing p (i, k, v) oa =
  case arr Array.! idx of
    (False, _, _) ->
      oa
        { oaArray = arr Array.// [(idx, (True, k, v))],
          oaMaxIters = max (oaMaxIters oa) $ succ $ fromIntegral p
        }
    _any -> insertOpenAdressing (succ p) (i, k, v) oa
  where
    s = fromIntegral $ length arr
    idx = fromIntegral $ (p + i) `mod` s
    arr = oaArray oa

openAdressing :: (Integral i) => (k -> i) -> Map k v -> OpenAddressing k v
openAdressing hash m
  | null m = OpenAddressing (Array.listArray (0, 0) []) 0
  | otherwise = foldr (insertOpenAdressing 0) oa ikv
  where
    oa = OpenAddressing arr 0
    ikv = (\(k, v) -> (hash k, k, v)) <$> Map.toList m
    size = 2 * Map.size m
    (fk, fv) = Map.findMin m
    arr =
      Array.listArray (0, size - 1) $
        replicate size (False, fk, fv)
