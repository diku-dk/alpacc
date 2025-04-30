module Alpacc.Util
  ( fixedPointIterate,
  )
where

import Data.Tuple.Extra (dupe)

-- | Performs fixed point iteration until a predicate holds true.
fixedPointIterate :: (Eq b) => (b -> b -> Bool) -> (b -> b) -> b -> b
fixedPointIterate cmp f = fst . head . dropWhile (uncurry cmp) . iterateFunction
  where
    iterateFunction = drop 1 . iterate swapApply . dupe
    swapApply (n, _) = (f n, n)
