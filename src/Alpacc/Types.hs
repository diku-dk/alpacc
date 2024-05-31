module Alpacc.Types
  ( UInt (..)
  , IInt (..)
  , toUInt
  , toIInt
  , toMaxUBound
  , toMaxIBound
  )
where

import Numeric.Natural
import Data.Word
import Data.Int

data UInt = U8 | U16 | U32 | U64 deriving (Eq, Ord, Show)

toUInt :: Natural -> Maybe UInt
toUInt i
  | i <= fromIntegral (maxBound :: Word8)  = Just U8
  | i <= fromIntegral (maxBound :: Word16) = Just U16
  | i <= fromIntegral (maxBound :: Word32) = Just U32
  | i <= fromIntegral (maxBound :: Word64) = Just U64
  | otherwise = Nothing

toMaxUBound :: UInt -> Natural
toMaxUBound U8 = fromIntegral (maxBound :: Word8)
toMaxUBound U16 = fromIntegral (maxBound :: Word16)
toMaxUBound U32 = fromIntegral (maxBound :: Word32)
toMaxUBound U64 = fromIntegral (maxBound :: Word64)

data IInt = I8 | I16 | I32 | I64 deriving (Eq, Ord, Show)

toIInt :: Integer -> Maybe IInt
toIInt i
  | fromIntegral (minBound :: Int8) <= i &&
    i <= fromIntegral (maxBound :: Int8)  = Just I8
  | fromIntegral (minBound :: Int16) <= i &&
    i <= fromIntegral (maxBound :: Int16) = Just I16
  | fromIntegral (minBound :: Int32) <= i &&
    i <= fromIntegral (maxBound :: Int32) = Just I32
  | fromIntegral (minBound :: Int64) <= i &&
    i <= fromIntegral (maxBound :: Int64) = Just I64
  | otherwise = Nothing

toMaxIBound :: IInt -> Natural
toMaxIBound I8 = fromIntegral (maxBound :: Int8)
toMaxIBound I16 = fromIntegral (maxBound :: Int16)
toMaxIBound I32 = fromIntegral (maxBound :: Int32)
toMaxIBound I64 = fromIntegral (maxBound :: Int64)
