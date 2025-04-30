module Alpacc.Generator.Futhark.Futharkify
  ( Futharkify (..),
    NTuple (..),
    RawString (..),
  )
where

import Alpacc.Types
import Data.Array as Array hiding (Array)
import Data.Array.IArray as IArray
import Data.Array.Unboxed (UArray)
import Data.Foldable
import Data.List qualified as List
import Data.String.Interpolate (i)
import Numeric.Natural

newtype NTuple a = NTuple [a] deriving (Show, Eq, Ord, Read, Foldable)

newtype RawString = RawString String deriving (Show, Eq, Ord, Read)

class Futharkify a where
  futharkify :: a -> String

instance Futharkify UInt where
  futharkify U8 = "u8"
  futharkify U16 = "u16"
  futharkify U32 = "u32"
  futharkify U64 = "u64"

instance Futharkify IInt where
  futharkify I8 = "i8"
  futharkify I16 = "i16"
  futharkify I32 = "i32"
  futharkify I64 = "i64"

instance Futharkify RawString where
  futharkify (RawString s) = s

instance Futharkify String where
  futharkify = show

instance Futharkify Int where
  futharkify = show

instance Futharkify Bool where
  futharkify True = "true"
  futharkify False = "false"

instance Futharkify Natural where
  futharkify = show

instance Futharkify Integer where
  futharkify = show

instance (Futharkify a, Futharkify b) => Futharkify (a, b) where
  futharkify (a, b) = [i|(#{futharkify a}, #{futharkify b})|]

instance (Futharkify a) => Futharkify [a] where
  futharkify = ("[" <>) . (<> "]") . List.intercalate ", " . fmap futharkify

instance (Futharkify a) => Futharkify (NTuple a) where
  futharkify =
    ("(" <>)
      . (<> ")")
      . List.intercalate ", "
      . fmap futharkify
      . toList

instance (Futharkify a) => Futharkify (Array i a) where
  futharkify = futharkify . Array.elems

instance (Futharkify a, IArray UArray a, Ix i) => Futharkify (UArray i a) where
  futharkify = futharkify . IArray.elems

instance (Futharkify a) => Futharkify (Maybe a) where
  futharkify (Just a) = "#some " <> futharkify a
  futharkify Nothing = "#none"
