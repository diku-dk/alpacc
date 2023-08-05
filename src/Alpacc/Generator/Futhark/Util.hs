module Alpacc.Generator.Futhark.Util
  ( lpad,
    rpad,
    maxFutUInt,
    selectFutUInt,
    toArray,
    futharkTableCase,
    futharkTableCases,
    toTupleIndexArray,
    indexArray,
    toTuple
  )
where

import Data.List qualified as List
import Data.String.Interpolate (i)

maxFutUInt :: FutUInt -> Integer
maxFutUInt U8 = 2^(8 :: Integer) - 1
maxFutUInt U16 = 2^(16 :: Integer) - 1
maxFutUInt U32 = 2^(32 :: Integer) - 1
maxFutUInt U64 = 2^(64 :: Integer) - 1

data FutUInt = U8 | U16 | U32 | U64 deriving (Ord, Eq, Bounded, Enum)

instance Show FutUInt where
  show U8 = "u8"
  show U16 = "u16"
  show U32 = "u32"
  show U64 = "u64"

selectFutUInt :: Integer -> Maybe FutUInt
selectFutUInt max_size
  | max_size < 0 = Nothing
  | max_size <= maxFutUInt U8 = Just U8
  | max_size <= maxFutUInt U16 = Just U16
  | max_size <= maxFutUInt U32 = Just U32
  | max_size <= maxFutUInt U64 = Just U64
  | otherwise = Nothing

-- | Adds m padding to the left side of a list.
lpad :: a -> Int -> [a] -> [a]
lpad p m xs = replicate (m - length ys) p ++ ys
  where
    ys = take m xs

-- | Adds m padding to the right side of a list.
rpad :: a -> Int -> [a] -> [a]
rpad p m xs = ys ++ replicate (m - length ys) p
  where
    ys = take m xs

-- | Adds square brackets to sides of a string.
squareBrackets :: String -> String
squareBrackets = ("[" ++) . (++ "]")

-- | Creates a string that is a array in the Futhark language.
toArray :: [String] -> String
toArray = squareBrackets . List.intercalate ", "

-- | Creates a string that is a single pattern matching in the Futhark Language.
-- This pattern matching is a table pair that results in a productions list.
futharkTableCase :: String -> String -> String
futharkTableCase k v = [i|case #{k} -> #{v}|]

-- | Creates a string that does pattern matching in the Futhark language.
-- The pattern matching is use to make the LLP table.
futharkTableCases :: [(String, String)] -> String
futharkTableCases = List.intercalate "\n  " . fmap (uncurry futharkTableCase)

-- | Creates a string that is a tuple where a variable is indexed from 0 to
-- n - 1 in the Futhark language.
toTupleIndexArray :: (Show a, Num a, Enum a) => String -> a -> String
toTupleIndexArray name n = toTuple $ map (indexArray name) [0 .. n - 1]

-- | Creates a string that indexes an array in the Futhark language.
indexArray :: Show a => String -> a -> String
indexArray name = (name ++) . squareBrackets . show

-- | Creates a string that is a tuple in the Futhark language.
toTuple :: [String] -> String
toTuple = ('(' :) . (++ ")") . List.intercalate ", "