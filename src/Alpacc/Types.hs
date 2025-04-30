module Alpacc.Types
  ( UInt (..),
    IInt (..),
    IntType (..),
  )
where

import Data.Composition
import Data.Function
import Data.Int
import Data.Word

data UInt = U64 | U32 | U16 | U8 deriving (Eq, Ord, Show, Enum, Bounded)

data IInt = I64 | I32 | I16 | I8 deriving (Eq, Ord, Show, Enum, Bounded)

class IntType a where
  intTypeMaxBound :: a -> Integer
  intTypeMinBound :: a -> Integer
  isIntType :: a -> Integer -> Bool
  isIntType a i = intTypeMinBound a <= i && i <= intTypeMaxBound a
  toIntType :: Integer -> Maybe a
  addIntType :: a -> Integer -> Integer -> Integer
  mulIntType :: a -> Integer -> Integer -> Integer
  modIntType :: a -> Integer -> Integer -> Integer

convertBinOp :: (Integral a) => (a -> a -> a) -> Integer -> Integer -> Integer
convertBinOp f = fromIntegral .: f `on` fromIntegral

instance IntType UInt where
  intTypeMaxBound U8 = fromIntegral (maxBound :: Word8)
  intTypeMaxBound U16 = fromIntegral (maxBound :: Word16)
  intTypeMaxBound U32 = fromIntegral (maxBound :: Word32)
  intTypeMaxBound U64 = fromIntegral (maxBound :: Word64)
  intTypeMinBound _ = 0
  toIntType i
    | isIntType U8 i = Just U8
    | isIntType U16 i = Just U16
    | isIntType U32 i = Just U32
    | isIntType U64 i = Just U64
    | otherwise = Nothing
  addIntType U8 = convertBinOp ((+) :: Word8 -> Word8 -> Word8)
  addIntType U16 = convertBinOp ((+) :: Word16 -> Word16 -> Word16)
  addIntType U32 = convertBinOp ((+) :: Word32 -> Word32 -> Word32)
  addIntType U64 = convertBinOp ((+) :: Word64 -> Word64 -> Word64)
  mulIntType U8 = convertBinOp ((*) :: Word8 -> Word8 -> Word8)
  mulIntType U16 = convertBinOp ((*) :: Word16 -> Word16 -> Word16)
  mulIntType U32 = convertBinOp ((*) :: Word32 -> Word32 -> Word32)
  mulIntType U64 = convertBinOp ((*) :: Word64 -> Word64 -> Word64)
  modIntType U8 = convertBinOp (mod :: Word8 -> Word8 -> Word8)
  modIntType U16 = convertBinOp (mod :: Word16 -> Word16 -> Word16)
  modIntType U32 = convertBinOp (mod :: Word32 -> Word32 -> Word32)
  modIntType U64 = convertBinOp (mod :: Word64 -> Word64 -> Word64)

instance IntType IInt where
  intTypeMaxBound I8 = fromIntegral (maxBound :: Int8)
  intTypeMaxBound I16 = fromIntegral (maxBound :: Int16)
  intTypeMaxBound I32 = fromIntegral (maxBound :: Int32)
  intTypeMaxBound I64 = fromIntegral (maxBound :: Int64)
  intTypeMinBound I8 = fromIntegral (minBound :: Int8)
  intTypeMinBound I16 = fromIntegral (minBound :: Int16)
  intTypeMinBound I32 = fromIntegral (minBound :: Int32)
  intTypeMinBound I64 = fromIntegral (minBound :: Int64)
  toIntType i
    | isIntType I8 i = Just I8
    | isIntType I16 i = Just I16
    | isIntType I32 i = Just I32
    | isIntType I64 i = Just I64
    | otherwise = Nothing
  addIntType I8 = convertBinOp ((+) :: Int8 -> Int8 -> Int8)
  addIntType I16 = convertBinOp ((+) :: Int16 -> Int16 -> Int16)
  addIntType I32 = convertBinOp ((+) :: Int32 -> Int32 -> Int32)
  addIntType I64 = convertBinOp ((+) :: Int64 -> Int64 -> Int64)
  mulIntType I8 = convertBinOp ((*) :: Int8 -> Int8 -> Int8)
  mulIntType I16 = convertBinOp ((*) :: Int16 -> Int16 -> Int16)
  mulIntType I32 = convertBinOp ((*) :: Int32 -> Int32 -> Int32)
  mulIntType I64 = convertBinOp ((*) :: Int64 -> Int64 -> Int64)
  modIntType I8 = convertBinOp (mod :: Int8 -> Int8 -> Int8)
  modIntType I16 = convertBinOp (mod :: Int16 -> Int16 -> Int16)
  modIntType I32 = convertBinOp (mod :: Int32 -> Int32 -> Int32)
  modIntType I64 = convertBinOp (mod :: Int64 -> Int64 -> Int64)
