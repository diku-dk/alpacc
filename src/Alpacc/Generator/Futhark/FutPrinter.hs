module Alpacc.Generator.Futhark.FutPrinter
  ( FutPrinter (..)
  , NTuple (..)
  , RawString (..)
  )
where

import Data.Foldable
import Data.List qualified as List
import Data.String.Interpolate (i)
import Data.Array (Array)
import Data.Array as Array hiding (Array)
import Data.Bits

newtype NTuple a = NTuple [a] deriving (Show, Eq, Ord, Read, Foldable)

newtype RawString = RawString String deriving (Show, Eq, Ord, Read)

class FutPrinter a where
  futPrint :: a -> String

instance FutPrinter String where
  futPrint = show

instance FutPrinter Int where
  futPrint = show
    -- where
    --   size = finiteBitSize (zeroBits :: Int)

instance FutPrinter RawString where
  futPrint (RawString s) = s

instance (FutPrinter a, FutPrinter b) => FutPrinter (a, b) where
  futPrint (a, b) = [i|(#{futPrint a}, #{futPrint b})|]

instance (FutPrinter a) => FutPrinter [a] where
  futPrint = ('[':) . (++"]") . List.intercalate ", " . fmap futPrint

instance (FutPrinter a) => FutPrinter (NTuple a) where
  futPrint =
    ('(':)
    . (++")")
    . List.intercalate ", "
    . fmap futPrint
    . toList

instance (FutPrinter a) => FutPrinter (Array i a) where
  futPrint = futPrint . Array.elems

instance (FutPrinter a) => FutPrinter (Maybe a) where
  futPrint (Just a) = "#some " ++ futPrint a
  futPrint Nothing = "#none"
