module ParallelParser.LL
  ( nullable,
    first,
    follow,
    last,
    before,
    fixedPointIterate,
    llTable,
    llParse,
  )
where

import qualified Data.List as List
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set (..))
import qualified Data.Set as Set
import Data.Tuple.Extra (dupe)
import ParallelParser.Grammar
import Prelude hiding (last)
import Debug.Trace (traceShow)

debug x = traceShow x x
fixedPointIterate :: (Show b, Eq b) => (b -> b -> Bool) -> (b -> b) -> b -> b
fixedPointIterate cmp f = fst . head . dropWhile (uncurry cmp) . iterateFunction
  where
    iterateFunction = drop 1 . iterate swapApply . dupe
    swapApply (n, _) = (f n, n)

nullables :: (Show t, Show nt, Ord nt, Ord t) => Grammar nt t -> Map nt Bool
nullables grammar = fixedPointIterate (/=) nullableNtProd init_nullable_map
  where
    nullableNtProd = nullableNt productions_map
    init_nullable_map = Map.fromList . map (,False) $ nonterminals grammar
    nullable' _ (Terminal _) = False
    nullable' nullable_map (Nonterminal a) = nullable_map Map.! a
    productions_map = toProductionsMap $ productions grammar
    nullableNt prods nullable_map = nullableNt' <$> prods
      where
        nullableNt' = any . all $ nullable' nullable_map

nullableOne :: (Show t, Show nt, Ord nt, Ord t) => Grammar nt t -> Symbol nt t -> Bool
nullableOne _ (Terminal t) = False
nullableOne grammar (Nonterminal nt) = nullable_map Map.! nt
  where
    nullable_map = nullables grammar

nullable :: (Show t, Show nt, Ord nt, Ord t) => Grammar nt t -> [Symbol nt t] -> Bool
nullable grammar = all nullableOne'
  where
    nullableOne' = nullableOne grammar

takeWhileNMore :: Int -> (a -> Bool) -> [a] -> [a]
takeWhileNMore _ _ [] = []
takeWhileNMore n predicate (x : xs)
  | predicate x = x : takeWhileNMore n predicate xs
  | otherwise = x : take (n - 1) xs

firstOne :: Ord nt => Map nt (Set [t]) -> Symbol nt t -> Set [t]
firstOne first_map (Terminal t) = Set.singleton [t]
firstOne first_map (Nonterminal nt) = first_map Map.! nt

truncatedProduct :: Ord t => Int -> Set [t] -> Set [t] -> Set [t]
truncatedProduct k a b = Set.fromList token_product
  where
    token_product = [take k $ ts ++ ts' | ts <- a_list, ts' <- b_list]
    a_list = Set.toList a
    b_list = Set.toList b

firstAB ::
  (Show t, Show nt, Ord nt, Ord t) =>
  Int ->
  Grammar nt t ->
  Map nt (Set [t]) ->
  [Symbol nt t] ->
  Set [t]
firstAB k grammar first_map = foldl truncatedProductSymbol init . kNullables
  where
    init = Set.singleton []
    truncatedProductSymbol a b = truncatedProduct k a (firstOne first_map b)
    kNullables = takeWhileNMore k nullableOne'
    nullableOne' = nullableOne grammar

firsts :: (Show t, Show nt, Ord nt, Ord t) => Int -> Grammar nt t -> Map nt (Set [t])
firsts k grammar = fixedPointIterate (/=) firstNtProd init_first_map
  where
    init = Set.singleton []
    firstNtProd = firstNt productions_map
    firstAB' first_map = Set.unions . fmap (firstAB k grammar first_map)
    init_first_map = Map.fromList . map (,init) $ nonterminals grammar
    productions_map = toProductionsMap $ productions grammar
    firstNt prods first_map = fmap (firstAB' first_map) prods

first :: (Show t, Show nt, Ord nt, Ord t) => Int -> Grammar nt t -> [Symbol nt t] -> Set [t]
first k grammar = Set.filter (not . null) . firstAB k grammar first_map
  where
    first_map = firsts k grammar

rightSymbols :: [Symbol nt t] -> [(nt, [Symbol nt t])]
rightSymbols [] = []
rightSymbols ((Terminal _) : xs) = rightSymbols xs
rightSymbols ((Nonterminal x) : xs) = (x, xs) : rightSymbols xs

data Constraint nt t
  = TConstraint (Set [t]) nt
  | NTConstraint nt nt
  deriving (Eq, Ord)

instance (Show nt, Show t) => Show (Constraint nt t) where
  show (TConstraint ts nt) = "{" ++ seq ++ "} ⊆ " ++ show nt
    where
      seq = List.intercalate ", " (show <$> Set.toList ts)
  show (NTConstraint nt nt') = show nt ++ " ⊆ " ++ show nt'

constraints :: (Show t, Show nt, Ord nt, Ord t) => Int -> Grammar nt t -> [Set (Constraint nt t)]
constraints k grammar = helper <$> productions grammar
  where
    nullable' = nullable grammar
    first' = first k grammar
    helper (Production nt s) = Set.unions $ uncurry auxiliary <$> right_symbols
      where
        right_symbols = rightSymbols s
        auxiliary nt' right = t_constraint `Set.union` nt_constraint
          where
            first_set = first' right
            right_nullable = nullable' right
            unempty = not . Set.null
            t_constraint =
              Set.fromList [TConstraint first_set nt' | unempty first_set]
            nt_constraint =
              Set.fromList [NTConstraint nt nt' | nt /= nt' && right_nullable]

follows :: (Show t, Show nt, Ord nt, Ord t) => Int -> Grammar nt t -> Map nt (Set [t])
follows k grammar = fixedPointIterate (/=) f init_follow_map
  where
    constraints' = Set.unions $ constraints k grammar
    init_follow_map = Map.fromList . map (,Set.empty) $ nonterminals grammar
    f a = foldl (flip addConstraint) a constraints'
    addConstraint (TConstraint t nt) m = Map.adjust (`Set.union` t) nt m
    addConstraint (NTConstraint nt nt') m = Map.adjust (`Set.union` res) nt' m
      where
        res = m Map.! nt

follow :: (Show t, Show nt, Ord nt, Ord t) => Int -> Grammar nt t -> nt -> Set [t]
follow k grammar = (follows' Map.!)
  where
    follows' = follows k grammar

last :: (Show t, Show nt, Ord nt, Ord t) => Int -> Grammar nt t -> [Symbol nt t] -> Set [t]
last q grammar = Set.map reverse . lasts . reverse
  where
    lasts = first q $ reverseGrammar grammar

before :: (Show t, Show nt, Ord nt, Ord t) => Int -> Grammar nt t -> nt -> Set [t]
before q grammar = Set.map reverse . (befores Map.!)
  where
    befores = follows q $ reverseGrammar grammar

firstTable :: (Show t, Show nt, Ord nt, Ord t) => Int -> Grammar nt t -> Map (nt, [t]) Int
firstTable k grammar = Map.unions $ auxiliary <$> zip prods [0 ..]
  where
    prods = productions grammar
    first' = first k grammar
    auxiliary (Production nt a, i) = Map.fromList [((nt, y), i) | y <- ts]
      where
        ts = Set.toList $ first' a

nullableFollowTable ::
  (Show t, Show nt, Ord nt, Ord t) =>
  Int ->
  Grammar nt t ->
  Map (nt, [t]) Int
nullableFollowTable k grammar = Map.unions $ auxiliary <$> zip [0 ..] prods
  where
    prods = productions grammar
    follow' = follow k grammar
    nullable' = nullable grammar
    auxiliary (i, Production nt a) =
      Map.fromList [((nt, y), i) | is_nullable, y <- nts]
      where
        nts = Set.toList $ follow' nt
        is_nullable = nullable' a

llTable :: (Show t, Show nt, Ord nt, Ord t) => Int -> Grammar nt t -> Map (nt, [t]) Int
llTable k grammar = Map.union first_table nullable_follow_table
  where
    first_table = firstTable k grammar
    nullable_follow_table = nullableFollowTable k grammar

llParse ::
  (Show t, Show nt, Ord nt, Ord t) =>
  Int ->
  Grammar nt t ->
  ([t], [Symbol nt t], [Int]) ->
  Maybe ([t], [Symbol nt t], [Int])
llParse k grammar = auxiliary
  where
    table = llTable k grammar
    production_map = Map.fromList . zip [0 ..] $ productions grammar
    auxiliary ([], stack, parsed) = Just ([], stack, reverse parsed)
    auxiliary (input, [], parsed) = Just (input, [], reverse parsed)
    auxiliary (x : xs, (Terminal y) : ys, parsed)
      | x == y = auxiliary (xs, ys, parsed)
      | otherwise = Nothing
    auxiliary (input, (Nonterminal y) : ys, parsed)
      | List.null keys = Nothing
      | isNothing maybeTuple = Nothing
      | otherwise = auxiliary (input, production ++ ys, index : parsed)
      where
        keys =
          List.filter (`Map.member` table)
            . fmap (y,)
            . takeWhile (not . List.null)
            . iterate init
            $ take k input

        maybeTuple = do
          index <- List.last keys `Map.lookup` table
          production <- index `Map.lookup` production_map
          return (index, symbols production)

        Just (index, production) = maybeTuple
