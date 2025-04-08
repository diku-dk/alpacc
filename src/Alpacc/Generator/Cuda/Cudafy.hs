module Alpacc.Generator.Cuda.Cudafy
  ( Cudafy (..),
    RawString (..),
  )
where

import Alpacc.Types
import Data.Array as Array hiding (Array)
import Data.Array.IArray as IArray
import Data.Array.Unboxed (UArray)
import Data.List qualified as List
import Numeric.Natural

newtype RawString = RawString String deriving (Show, Eq, Ord, Read)

class Cudafy a where
  cudafy :: a -> String

instance Cudafy UInt where
  cudafy U8 = "unsigned char"
  cudafy U16 = "unsigned short"
  cudafy U32 = "unsigned int"
  cudafy U64 = "unsigned long long"

instance Cudafy IInt where
  cudafy I8 = "char"
  cudafy I16 = "short"
  cudafy I32 = "int"
  cudafy I64 = "long long"

instance Cudafy RawString where
  cudafy (RawString s) = s

instance Cudafy String where
  cudafy = show

instance Cudafy Int where
  cudafy = show

instance Cudafy Bool where
  cudafy True = "true"
  cudafy False = "false"

instance Cudafy Natural where
  cudafy = show

instance Cudafy Integer where
  cudafy = show

instance (Cudafy a) => Cudafy [a] where
  cudafy = ("{" <>) . (<> "}") . List.intercalate ", " . fmap cudafy

instance (Cudafy a) => Cudafy (Array i a) where
  cudafy = cudafy . Array.elems

instance (Cudafy a, IArray UArray a, Ix i) => Cudafy (UArray i a) where
  cudafy = cudafy . IArray.elems
