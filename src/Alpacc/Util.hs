module Alpacc.Util
  ( fixedPointIterate,
    listProducts,
  )
where

import Combinatorics (variateRep)
import Data.Maybe

-- | Performs fixed point iteration until a predicate holds true.
fixedPointIterate :: (Eq b) => (b -> b -> Bool) -> (b -> b) -> b -> b
fixedPointIterate cmp f = auxiliary
  where
    auxiliary n =
      if n' `cmp` n
        then
          n'
        else
          auxiliary n'
      where
        n' = f n

listProducts :: Int -> [a] -> [[a]]
listProducts i = map catMaybes . variateRep i . map Just
