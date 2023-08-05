module Alpacc.Generator.Futhark.Util
  ( lpad,
    rpad,
    maxFutUInt,
    selectFutUInt
  )
where

maxFutUInt :: FutUInt -> Integer
maxFutUInt U8 = 255
maxFutUInt U16 = 65535
maxFutUInt U32 = 4294967295
maxFutUInt U64 = 18446744073709551615

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
