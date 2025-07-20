module Alpacc.Util
  ( fixedPointIterate,
    toWord8s,
  )
where

import Codec.Binary.UTF8.String (encodeChar)
import Data.Tuple.Extra (dupe)
import Data.Word

-- | Performs fixed point iteration until a predicate holds true.
fixedPointIterate :: (Eq b) => (b -> b -> Bool) -> (b -> b) -> b -> b
fixedPointIterate cmp f = fst . head . dropWhile (uncurry cmp) . iterateFunction
  where
    iterateFunction = drop 1 . iterate swapApply . dupe
    swapApply (n, _) = (f n, n)

toWord8s :: String -> [Word8]
toWord8s = concatMap encodeChar
