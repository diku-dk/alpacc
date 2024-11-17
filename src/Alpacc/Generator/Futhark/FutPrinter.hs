module Alpacc.Generator.Futhark.FutPrinter
  ( FutPrinter (..)
  , NTuple (..)
  , RawString (..)
  )
where

import Data.Foldable
import Data.Array as Array hiding (Array)
import Data.List qualified as List
import Data.Array.Unboxed (UArray)
import Data.Array.IArray as IArray
import Data.String.Interpolate ( i )
import Alpacc.Types
import Numeric.Natural

newtype NTuple a = NTuple [a] deriving (Show, Eq, Ord, Read, Foldable)

newtype RawString = RawString String deriving (Show, Eq, Ord, Read)

class FutPrinter a where
  futPrint :: a -> String

instance FutPrinter UInt where
  futPrint U8 = "u8"
  futPrint U16 = "u16"
  futPrint U32 = "u32"
  futPrint U64 = "u64"

instance FutPrinter IInt where
  futPrint I8 = "i8"
  futPrint I16 = "i16"
  futPrint I32 = "i32"
  futPrint I64 = "i64"

instance FutPrinter RawString where
  futPrint (RawString s) = s

instance FutPrinter String where
  futPrint = show

instance FutPrinter Int where
  futPrint = show

instance FutPrinter Bool where
  futPrint True = "true"
  futPrint False = "false"

instance FutPrinter Natural where
  futPrint = show

instance FutPrinter Integer where
  futPrint = show

instance (FutPrinter a, FutPrinter b) => FutPrinter (a, b) where
  futPrint (a, b) = [i|#{futPrint a}, #{futPrint b})|]

instance (FutPrinter a) => FutPrinter [a] where
  futPrint = ("["<>) . (<>"]") . List.intercalate ", " . fmap futPrint

instance (FutPrinter a) => FutPrinter (NTuple a) where
  futPrint =
    ("("<>)
    . (<>")")
    . List.intercalate ", "
    . fmap futPrint
    . toList

instance (FutPrinter a) => FutPrinter (Array i a) where
  futPrint = futPrint . Array.elems

instance (FutPrinter a, IArray UArray a, Ix i) => FutPrinter (UArray i a) where
  futPrint = futPrint . IArray.elems

instance (FutPrinter a) => FutPrinter (Maybe a) where
  futPrint (Just a) = "#some " <> futPrint a
  futPrint Nothing = "#none"
