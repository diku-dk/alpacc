module Alpacc.Generator.Cuda.Cudafy
  ( Cudafy (..),
    RawString (..),
  )
where

import Alpacc.Types
import Data.Array as Array hiding (Array)
import Data.Array.IArray as IArray
import Data.Array.Unboxed (UArray)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural

newtype RawString = RawString Text deriving (Show, Eq, Ord, Read)

class Cudafy a where
  cudafy :: a -> Text

instance Cudafy UInt where
  cudafy U8 = "uint8_t"
  cudafy U16 = "uint16_t"
  cudafy U32 = "uint32_t"
  cudafy U64 = "uint64_t"

instance Cudafy IInt where
  cudafy I8 = "int8_t"
  cudafy I16 = "int16_t"
  cudafy I32 = "int32_t"
  cudafy I64 = "int64_t"

instance Cudafy RawString where
  cudafy (RawString s) = s

instance Cudafy String where
  cudafy = Text.pack . show

instance Cudafy Int where
  cudafy = Text.pack . show

instance Cudafy Bool where
  cudafy True = "true"
  cudafy False = "false"

instance Cudafy Natural where
  cudafy = Text.pack . show

instance Cudafy Integer where
  cudafy = Text.pack . show

instance (Cudafy a) => Cudafy [a] where
  cudafy = ("{" <>) . (<> "}") . Text.intercalate ", " . fmap cudafy

instance (Cudafy a) => Cudafy (Array i a) where
  cudafy = cudafy . Array.elems

instance (Cudafy a, IArray UArray a, Ix i) => Cudafy (UArray i a) where
  cudafy = cudafy . IArray.elems

instance (Cudafy a, Cudafy b) => Cudafy (a, b) where
  cudafy (a, b) = Text.pack [i|{#{cudafy a}, #{cudafy b}}|]
