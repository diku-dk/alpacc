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
  (Ord nt, Ord t) =>
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
futharkKeyGeneration :: (Ord nt, Ord t, Show nt, Show t) => Int -> Int -> Grammar nt t -> Maybe String
futharkKeyGeneration q k grammar
  | any_is_nothing = Nothing
  | otherwise =
      Just
        [i|import "lib/github.com/diku-dk/sorts/quick_sort"

def lookbkack_array_to_tuple [n] (arr : [n]u32) : (u32) =
  (arr[0])

def lookahead_array_to_tuple [n] (arr : [n]u32) : (u32) =
  (arr[0])

def lookback_chunks [n] (arr : [n]u32) =
  let arr' = replicate 1 u32.highest ++ arr
  in iota n |> map (\i -> arr'[i:i + 1] |> lookbkack_array_to_tuple)

def lookahead_chunks [n] (arr : [n]u32) =
  let arr' = arr ++ replicate 1 u32.highest
  in iota n |> map (\i -> arr'[i:i + 1] |> lookahead_array_to_tuple)

def keys [n] (arr : [n]u32) =
  let lookback = lookback_chunks arr
  let lookahead = lookahead_chunks arr
  in zip lookback lookahead

def key_to_config (key : (#{lookback_type}, #{lookahead_type})) =
  match key
  #{futhark_table}

type bracket = #left u64 | #right u64

def lbr (xs : []u64) : []bracket =
  map (\x -> #left x) xs

def rbr (xs : []u64) : []bracket =
  map (\x -> #right x) xs

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

def to_tuple (b : bracket) : i64 =
  match b
  case #left _ -> 1
  case #right _ -> -1

def is_left (b : bracket) : bool =
  match b
  case #left _ -> true
  case #right _ -> false

def is_balanced (input : []bracket) =
  input
  |> map to_tuple
  |> scan (+) 0
  |> (\\a -> all (0<=) a && last a == 0)

def unpack_bracket (b : bracket) : u64 =
  match b
  case #left a -> a
  case #right a -> a

def eq_bracket(a : bracket) (b : bracket) : bool =
  match (a, b)
  case (#left _, #right _) -> false
  case (#right _, #left _) -> false
  case (#left x, #left y) -> x u64.== y
  case (#right x, #right y) -> x u64.== y

def lq_bracket (a : bracket) (b : bracket) : bool =
  match (a, b)
  case (#left _, #right _) -> true
  case (#right _, #left _) -> false
  case (#left x, #left y) -> x u64.<= y
  case (#right x, #right y) -> x u64.<= y

def lq_bracket_indexed (a : (bracket, i64)) (b : (bracket, i64)) : bool =
  if eq_bracket a.0 b.0
  then a.1 i64.<= b.1
  else lq_bracket a.0 b.0

def eq_no_bracket (a : bracket) (b : bracket) : bool =
  unpack_bracket a u64.== unpack_bracket b

def zip_index 'a [n] (as : [n]a) : [n](a, i64) =
  zip as (iota n)

def is_correctly_typed [n] (brackets : [n]bracket) =
  let sorted_brackets =
    brackets
    |> zip_index
    |> qsort lq_bracket_indexed
    |> map (.0)
    |> map unpack_bracket
  let n_halves = n / 2
  let left = sorted_brackets[n_halves:] :> [n_halves]u64
  let right = sorted_brackets[:n_halves] :> [n_halves]u64
  in map2 (==) left right |> and

def bracket_matching (brackets : []bracket) : bool =
  if is_balanced brackets |> not
  then false
  else is_correctly_typed brackets

def parse [n] (arr : [n]u32) =
  let arr' = [0] ++ (map (+2) arr) ++ [1]
  let configs = keys arr' |> map key_to_config
  let brackets = step_one configs
  in if bracket_matching brackets
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