{-# LANGUAGE TemplateHaskell #-}
module ParallelParser.Generator
  ( futharkKeyGeneration,
  )
where

import Data.FileEmbed
import qualified Data.Bifunctor as BI
import Data.Composition
import qualified Data.List as L
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String.Interpolate (i)
import Data.Tuple.Extra
import ParallelParser.Grammar
import ParallelParser.LLP
    ( Bracket(..), llpParserTableWithStartsHomomorphisms )
import Control.DeepSeq
import Data.Either.Extra (maybeToEither)

-- import Debug.Trace (traceShow)
-- debug x = traceShow x x

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
    terminal_map = Map.fromList . flip zip (show <$> [(0::Int) ..]) $ terminals grammar
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
    snd' = BI.second (toTuple . rpad "u32.highest" max_pi . map show)
    fst' = BI.first (toTuple . rpad "#epsilon" max_alpha_omega . map auxiliary)
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
  (Int, Int, String, String)
futharkTable q k grammar table =
  (max_alpha_omega,max_pi,ne,) .
  (++last_case_str) . cases . prods . keys $ symbolsToInts' table
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

futharkParser :: String
futharkParser = $(embedStringFile "fut/parser.fut")

-- | Creates Futhark source code which contains a parallel parser that can
-- create the productions list for a input which is indexes of terminals.
futharkKeyGeneration ::
  (Ord nt, Ord t, Show nt, Show t, NFData t, NFData nt) =>
  Int -> Int -> Grammar nt t -> Either String String
futharkKeyGeneration q k grammar = do
  start_terminal <- maybeToEither "" maybe_start_terminal
  end_terminal <- maybeToEither "" maybe_end_terminal
  table <- maybe_table
  let (max_ao, max_pi, ne, futhark_table) =
        futharkTable q k augmented_grammar table
      brackets_ty = "(" <> List.intercalate "," (replicate max_ao "bracket") <> ")"
      productions_ty = "(" <> List.intercalate "," (replicate max_pi "u32") <> ")"
      brackets = List.intercalate "," $ zipWith (<>) (replicate max_ao "b") $ map show [(0::Int)..]
      productions = List.intercalate "," $ zipWith (<>) (replicate max_pi "p") $ map show [(0::Int)..]
  return $ futharkParser <>
    [i|
module parser = mk_parser {

type lookahead_type = #{lookahead_type}
type lookback_type = #{lookback_type}

def q : i64 = #{q}
def k : i64 = #{k}
def max_ao : i64 = #{max_ao}
def max_pi : i64 = #{max_pi}
def start_terminal : terminal = #{start_terminal}
def end_terminal : terminal = #{end_terminal}

def lookback_array_to_tuple [n] (arr : [n]u32) =
  #{toTupleIndexArray "arr" q}

def lookahead_array_to_tuple [n] (arr : [n]u32) =
  #{toTupleIndexArray "arr" k}

def key_to_config (key : (lookback_type, lookahead_type))
                : maybe ([max_ao]bracket, [max_pi]u32) =
  (\\r -> match (r: maybe (#{brackets_ty}, #{productions_ty}))
          case #nothing -> #nothing
          case #just ((#{brackets}),(#{productions})) ->
            #just (sized max_ao [#{brackets}], sized max_pi [#{productions}])) <|
  match key
  #{futhark_table}

def ne : ([max_ao]bracket, [max_pi]u32) =
  let (a,b) = #{ne}
  in (sized max_ao a, sized max_pi b)
}
|]
  where
    maybe_start_terminal = List.elemIndex RightTurnstile terminals' :: Maybe Int
    maybe_end_terminal = List.elemIndex LeftTurnstile terminals' :: Maybe Int
    augmented_grammar = augmentGrammar grammar
    maybe_table = llpParserTableWithStartsHomomorphisms q k grammar
    terminals' = terminals augmented_grammar
    lookback_type = toTuple $ replicate q "u32"
    lookahead_type = toTuple $ replicate k "u32"
