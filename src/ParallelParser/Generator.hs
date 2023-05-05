module ParallelParser.Generator
  ( futharkKeyGeneration,
  )
where

import Data.Bifunctor
import Data.Composition
import qualified Data.List as L
import qualified Data.List as List
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Maybe
import Data.String.Interpolate (i)
import Data.Tuple.Extra
import ParallelParser.Grammar
import ParallelParser.LLP
import Debug.Trace (traceShow)
debug x = traceShow x x

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

-- | Given the table keys for a LLP parser create the keys which will be used
-- in the Futhark language for pattern matching.
futharkTableKey ::
  Ord t =>
  Int ->
  Int ->
  Grammar nt t ->
  [t] ->
  [t] ->
  (String, String)
futharkTableKey q k grammar = both toTuple . bimap backPad frontPad . both convert .: (,)
  where
    terminal_map = Map.fromList . flip zip (show <$> [0 ..]) $ terminals grammar
    convert = map (terminal_map Map.!)
    backPad = lpad "4294967295" q
    frontPad = rpad "4294967295" k

-- | Adds square brackets to sides of a string.
squareBrackets :: String -> String
squareBrackets = ("[" ++) . (++ "]")

-- | Creates a string that indexes an array in the Futhark language.
indexArray :: Show a => String -> a -> String
indexArray name = (name ++) . squareBrackets . show

-- | Creates a string that is a tuple in the Futhark language.
toTuple :: [String] -> String
toTuple = ('(' :) . (++ ")") . List.intercalate ", "

-- | Creates a string that is a array in the Futhark language.
toArray :: [String] -> String
toArray = squareBrackets . List.intercalate ", "

-- | Creates a string that is a array in the Futhark language which corresponds
-- to the resulting productions list. This is used in the pattern matching.
futharkProductions :: Show a => Int -> [a] -> String
futharkProductions max_size = toArray . lpad "u32.highest" max_size . map show

-- | Creates a string that is a tuple where a variable is indexed from 0 to 
-- n - 1 in the Futhark language.
toTupleIndexArray :: (Show a, Num a, Enum a) => String -> a -> String
toTupleIndexArray name n = toTuple $ map (indexArray name) [0 .. n - 1]

-- | Creates a string that is a single pattern matching in the Futhark Language.
-- This pattern matching is a table pair that results in a productions list.
futharkTableCase :: (String, String) -> String -> String
futharkTableCase (b, f) v = [i|case (#{b}, #{f}) -> #{v}|]


-- | Creates a string that does pattern matching in the Futhark language.
-- The pattern matching is use to make the LLP table.
futharkTablesCases :: [((String, String), String)] -> String
futharkTablesCases = L.intercalate "\n  " . fmap (uncurry futharkTableCase)

-- | Creates a string that is the resulting LLP table which is done by using
-- pattern matching in Futhark.
futharkTable ::
  (Ord t, Show a) =>
  Int ->
  Int ->
  Grammar nt t ->
  Int ->
  Map ([t], [t]) [a] ->
  [Char]
futharkTable q k grammar max_size = cases . prods . keys
  where
    cases = futharkTablesCases . Map.toList
    prods = fmap (futharkProductions max_size)
    keys = Map.mapKeys (uncurry (futharkTableKey q k grammar))

-- | Creates Futhark source code which contains a parallel parser that can
-- create the productions list for a input which is indexes of terminals.
futharkKeyGeneration :: (Ord nt, Ord t, Show nt, Show t) => Int -> Int -> Grammar nt t -> Maybe String
futharkKeyGeneration q k grammar
  | any_is_nothing = Nothing
  | otherwise =
      Just
        [i|def lookbkack_array_to_tuple [n] (arr : [n]u32) =
  #{toTupleIndexArray "arr" q}

def lookahead_array_to_tuple [n] (arr : [n]u32) =
  #{toTupleIndexArray "arr" k}

def lookback_chunks [n] (arr : [n]u32) =
  let arr' = replicate #{q} u32.highest ++ arr
  in iota n |> map (\\i -> arr'[i:i + #{q}] |> lookbkack_array_to_tuple)

def lookahead_chunks [n] (arr : [n]u32) =
  let arr' = arr ++ replicate #{k} u32.highest
  in iota n |> map (\\i -> arr'[i:i + #{k}] |> lookahead_array_to_tuple)

def keys [n] (arr : [n]u32) =
  let lookback = lookback_chunks arr
  let lookahead = lookahead_chunks arr
  in zip lookback lookahead

def key_to_productions (key : (#{lookback_type}, #{lookahead_type})) =
  match key
  #{futhark_table}
  case _ -> assert false #{last_case}

def parse [n] (arr : [n]u32) =
  let arr' = #{start_terminal} ++ (map (+2) arr) ++ #{end_terminal}
  in keys arr'
  |> map key_to_productions
  |> flatten
  |> filter (!=u32.highest)
  |> tail
  |> map (\\a -> a - 1)
|]
  where
    any_is_nothing =
      isNothing maybe_start_terminal
        || isNothing maybe_end_terminal
        || isNothing maybe_table
    Just start_terminal = List.singleton <$> maybe_start_terminal
    Just end_terminal = List.singleton <$> maybe_end_terminal
    Just table = fmap thd3 <$> maybe_table
    maybe_start_terminal = List.elemIndex RightTurnstile terminals'
    maybe_end_terminal = List.elemIndex LeftTurnstile terminals'
    augmented_grammar = augmentGrammar grammar
    maybe_table = llpParsingTable q k grammar
    terminals' = terminals augmented_grammar
    lookback_type = toTuple $ replicate q "u32"
    lookahead_type = toTuple $ replicate k "u32"
    max_size = maximum $ length <$> table
    futhark_table = futharkTable q k augmented_grammar max_size table
    last_case = toArray $ replicate max_size "u32.highest"