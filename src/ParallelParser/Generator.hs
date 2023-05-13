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
futharkProductions :: Int -> Int -> Int -> ([Int], [Int], [Int]) -> String
futharkProductions max_alpha max_omega max_pi = toTuple . toArr . thd' . snd' . fst'
  where
    toArr (a, b, c) = [a, b, c]
    fst' = first3 (auxliary "u64.highest" max_alpha)
    snd' = second3 (auxliary "u64.highest" max_omega)
    thd' = third3 (auxliary "u32.highest" max_pi)
    auxliary str max = toArray . rpad str max . map show

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
  Map ([t], [t]) ([Symbol nt t], [Symbol nt t], [Int]) ->
  Map ([t], [t]) ([Int], [Int], [Int])
symbolsToInts grammar = Map.map auxiliary
  where
    ts = Terminal <$> terminals grammar
    nts = Nonterminal <$> nonterminals grammar
    mapping = Map.fromList $ zip (ts ++ nts) [0..]
    auxiliary (a, b, c) = (fmap (mapping Map.!) a, fmap (mapping Map.!) b, c)

-- | Creates a string that is the resulting LLP table which is done by using
-- pattern matching in Futhark.
futharkTable ::
  (Ord nt, Show nt, Show t, Ord t, NFData t, NFData nt) =>
  Int ->
  Int ->
  Grammar nt t ->
  Map ([t], [t]) ([Symbol nt t], [Symbol nt t], [Int]) ->
  (Int, String)
futharkTable q k grammar table = (max_alpha_omega, ) . (++last_case_str) . cases . prods . keys $ symbolsToInts' table
  where
    symbolsToInts' = symbolsToInts grammar
    cases = futharkTablesCases . Map.toList
    values = Map.elems table
    max_alpha = maximum $ length . fst3 <$> values
    max_omega = maximum $ length . snd3 <$> values
    max_alpha_omega = max max_alpha max_omega
    alpha_omega_size = max_alpha + max_omega
    max_pi = maximum $ length . thd3 <$> values
    stacks = toArray . flip replicate "u64.highest" <$> [max_alpha_omega, max_alpha_omega]
    rules = toArray $ replicate max_pi "u32.highest"
    last_case = toTuple (stacks ++ [rules])
    last_case_str = [i|\n  case _ -> assert false #{last_case}|]
    prods = fmap (futharkProductions max_alpha_omega max_alpha_omega max_pi)
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

def key_to_config (key : (#{lookback_type}, #{lookahead_type})) =
  match key
  #{futhark_table}

type bracket = #left u64 | #right u64
type maybe 'a = #just a | #nothing

def lbr (xs : []u64) : []bracket =
  map (\\x -> #left x) xs

def rbr (xs : []u64) : []bracket =
  map (\\x -> #right x) xs

def is_nonempty_bracket (b : bracket) =
  match b
  case #left 18446744073709551615 -> false
  case #right 18446744073709551615 -> false
  case _ -> true

def eval_homomorphisms (config : ([]u64, []u64, []u32)) : []bracket =
  let reverse_omega = reverse config.1
  in rbr config.0 ++ lbr reverse_omega

def step_one [n][m] (configs : []([n]u64, [m]u64, []u32)) =
  let x = head configs
  let xs = tail configs
  let k = n + m
  let evaluated_homomorphisms = map (\\a -> eval_homomorphisms a :> [k]bracket) xs |> flatten
  let lbr_reverse_omega = reverse x.1 |> lbr
  in (lbr_reverse_omega ++ evaluated_homomorphisms)
    |> filter is_nonempty_bracket

def is_left (b : bracket) : bool =
  match b
  case #left _ -> true
  case #right _ -> false

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
  in if any (<0) bracket_scan
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

def parse [n] (arr : [n]u32) =
  let arr' = [0] ++ (map (+2) arr) ++ [1]
  let configs = keys arr' |> map key_to_config
  let brackets = step_one configs
  in if brackets_matches brackets
  then map (.2) configs
    |> flatten
    |> filter (!=u32.highest)
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
    maybe_table = llpParsingTable q k grammar
    terminals' = terminals augmented_grammar
    lookback_type = toTuple $ replicate q "u32"
    lookahead_type = toTuple $ replicate k "u32"
    (max_alpha_omega, futhark_table) = futharkTable q k augmented_grammar table