module ParallelParser.Generator
  ( futharkKeyGeneration,
  )
where

import qualified Data.Bifunctor as BI
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
import Control.DeepSeq
import GHC.Generics

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
futharkTableKey q k grammar = both toTuple . BI.bimap backPad frontPad . both convert .: (,)
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
futharkProductions :: Int -> Int -> ([Bracket Int], [Int]) -> String
futharkProductions max_alpha_omega max_pi = ("#just " ++) . toTuple . toArr . snd' . fst'
  where
    toArr (a, b) = [a, b]
    snd' = BI.second (toArray . rpad "u32.highest" max_pi . map show)
    fst' = BI.first (toArray . rpad "#epsilon" max_alpha_omega . map auxiliary)
    auxiliary (LBracket a) = "#left " ++ show a
    auxiliary (RBracket a) = "#right " ++ show a

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

symbolsToInts ::
  (Ord nt, Ord t) =>
  Grammar nt t ->
  Map ([t], [t]) ([Bracket (Symbol nt t)], [Int]) ->
  Map ([t], [t]) ([Bracket Int], [Int])
symbolsToInts grammar = Map.map auxiliary
  where
    ts = Terminal <$> terminals grammar
    nts = Nonterminal <$> nonterminals grammar
    mapping = Map.fromList $ zip (ts ++ nts) [0..]
    auxiliary (a, b) = (fmap (mapping Map.!) <$> a, b)

-- | Creates a string that is the resulting LLP table which is done by using
-- pattern matching in Futhark.
futharkTable ::
  (Ord nt, Ord t) =>
  Int ->
  Int ->
  Grammar nt t ->
  Map ([t], [t]) ([Bracket (Symbol nt t)], [Int]) ->
  (String, String)
futharkTable q k grammar table = (ne,) . (++last_case_str) . cases . prods . keys $ symbolsToInts' table
  where
    symbolsToInts' = symbolsToInts grammar
    cases = futharkTablesCases . Map.toList
    values = Map.elems table
    max_alpha_omega = maximum $ length . fst <$> values
    max_pi = maximum $ length . snd <$> values
    stacks = toArray $ replicate max_alpha_omega "#epsilon"
    rules = toArray $ replicate max_pi "u32.highest"
    ne = toTuple [stacks, rules]
    last_case_str = [i|\n  case _ -> #nothing|]
    prods = fmap (futharkProductions max_alpha_omega max_pi)
    keys = Map.mapKeys (uncurry (futharkTableKey q k grammar))

-- | Creates Futhark source code which contains a parallel parser that can
-- create the productions list for a input which is indexes of terminals.
futharkKeyGeneration ::
  (Ord nt, Ord t, Show nt, Show t, NFData t, NFData nt) =>
  Int -> Int -> Grammar nt t -> Maybe String
futharkKeyGeneration q k grammar
  | any_is_nothing = Nothing
  | otherwise =
      Just
        [i|import "lib/github.com/diku-dk/sorts/radix_sort"

type bracket = #left u64 | #right u64 | #epsilon
type maybe 'a = #just a | #nothing

def lookbkack_array_to_tuple [n] (arr : [n]u32) =
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

def key_to_config (key : (#{lookback_type}, #{lookahead_type})) : maybe ([]bracket, []u32) =
  match key
  #{futhark_table}

def is_left (b : bracket) : bool =
  match b
  case #left _ -> true
  case #right _ -> false
  case #epsilon -> false

def depths [n] (input : [n]bracket) : maybe ([n]i64) =
  let left_brackets =
    input
    |> map (is_left)
  let bracket_scan =
    left_brackets
    |> map (\\b -> if b then 1 else -1)
    |> scan (+) 0
  let result =
    bracket_scan
    |> map2 (\\a b -> b - i64.bool a) left_brackets
  in if any (<0) bracket_scan || last bracket_scan != 0
  then #nothing
  else #just result

def grade (xs : []i64) =
  zip xs (indices xs)
  |> radix_sort_int_by_key (.0) i64.num_bits i64.get_bit
  |> map (.1)

def even_indices 'a [n] (_ : [n]a) =
  iota (n / 2) |> map (2*)

def unpack_bracket (b : bracket) : u64 =
  match b
  case #left a -> a
  case #right a -> a
  case #epsilon -> assert false 0

def eq_no_bracket (a : bracket) (b : bracket) : bool =
  unpack_bracket a u64.== unpack_bracket b

def brackets_matches [n] (brackets : [n]bracket) =
  match depths brackets
  case #just depths' ->
    let grade' = grade depths'
    in even_indices grade'
      |> map (\\i -> eq_no_bracket brackets[grade'[i]] brackets[grade'[i+1]])
      |> and
  case #nothing -> false

def is_nothing 'a (m : maybe a) : bool =
  match m
  case #just _ -> false
  case #nothing -> true

def from_just 'a (ne : a) (m : maybe a) : a =
  match m
  case #just a -> a
  case _ -> ne

entry parse [n] (arr : [n]u32) : []u32 =
  let arr' = [0] ++ (map (+2) arr) ++ [1]
  let configs = keys arr' |> map key_to_config
  in if any (is_nothing) configs
  then []
  else
  let ne = #{ne}
  let (brackets, productions) = configs |> map (from_just ne) |> unzip
  in if brackets 
        |> flatten
        |> filter (!=#epsilon)
        |> brackets_matches
  then productions
    |> flatten
    |> filter (!=u32.highest)
    |> tail
    |> map (\\a -> a - 1)
  else []
|]
  where
    any_is_nothing =
      isNothing maybe_start_terminal
        || isNothing maybe_end_terminal
        || isNothing maybe_table
    Just start_terminal = List.singleton <$> maybe_start_terminal
    Just end_terminal = List.singleton <$> maybe_end_terminal
    Just table = maybe_table
    maybe_start_terminal = List.elemIndex RightTurnstile terminals'
    maybe_end_terminal = List.elemIndex LeftTurnstile terminals'
    augmented_grammar = augmentGrammar grammar
    maybe_table = llpParserTableWithStartsHomomorphisms q k grammar
    terminals' = terminals augmented_grammar
    lookback_type = toTuple $ replicate q "u32"
    lookahead_type = toTuple $ replicate k "u32"
    (ne, futhark_table) = futharkTable q k augmented_grammar table