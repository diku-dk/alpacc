module Alpacc.Types
  ( UInt (..)
  , toUInt
  , toMaxBound
  )
where

import Numeric.Natural
import Data.Word

data UInt = U8 | U16 | U32 | U64 deriving (Eq, Ord, Show)

toUInt :: Natural -> Maybe UInt
toUInt i
  | i < fromIntegral (maxBound :: Word8)  = Just U8
  | i < fromIntegral (maxBound :: Word16) = Just U16
  | i < fromIntegral (maxBound :: Word32) = Just U32
  | i < fromIntegral (maxBound :: Word64) = Just U64
  | otherwise = Nothing

toMaxBound :: UInt -> Natural
toMaxBound U8 = fromIntegral (maxBound :: Word8)
toMaxBound U16 = fromIntegral (maxBound :: Word16)
toMaxBound U32 = fromIntegral (maxBound :: Word32)
toMaxBound U64 = fromIntegral (maxBound :: Word64)
