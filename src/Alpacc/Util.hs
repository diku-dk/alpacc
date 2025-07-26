module Alpacc.Util
  ( fixedPointIterate,
    toWord8s,
    listProducts,
  )
where

import Codec.Binary.UTF8.String (encodeChar)
import Data.Word

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

toWord8s :: String -> [Word8]
toWord8s = concatMap encodeChar

listProducts :: Int -> [a] -> [[a]]
listProducts i = concat . zipWith auxiliary [0 .. i] . repeat
  where
    auxiliary j = foldr (liftA2 (:)) [[]] . replicate j
