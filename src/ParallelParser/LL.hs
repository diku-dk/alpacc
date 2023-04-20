module ParallelParser.LL
  ( nullable,
    first,
    follow,
    last,
    before,
    fixedPointIterate,
    llTable,
    llParse,
    firstTable,
    nullableFollowTable
  )
where

import qualified Data.List as List
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set (..))
import qualified Data.Set as Set
import Data.Tuple.Extra (dupe, both)
import ParallelParser.Grammar
import Prelude hiding (last)
import Data.Function
import Debug.Trace (traceShow)

debug x = traceShow x x


fixedPointIterate :: Eq b => (b -> b -> Bool) -> (b -> b) -> b -> b
fixedPointIterate cmp f = fst . head . dropWhile (uncurry cmp) . iterateFunction
  where
    iterateFunction = drop 1 . iterate swapApply . dupe
    swapApply (n, _) = (f n, n)

nullables :: (Ord nt, Ord t) => Grammar nt t -> Map nt Bool
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

nullableOne :: (Ord nt, Ord t) => Grammar nt t -> Symbol nt t -> Bool
nullableOne _ (Terminal t) = False
nullableOne grammar (Nonterminal nt) = nullable_map Map.! nt
  where
    nullable_map = nullables grammar

nullable :: (Ord nt, Ord t) => Grammar nt t -> [Symbol nt t] -> Bool
nullable grammar = all nullableOne'
  where
    nullableOne' = nullableOne grammar

takeWhileNMore :: Int -> (a -> Bool) -> [a] -> [a]
takeWhileNMore _ _ [] = []
takeWhileNMore n predicate (x : xs)
  | predicate x = x : takeWhileNMore n predicate xs
  | otherwise = x : take (n - 1) xs

-- firstOne :: Ord nt => Map nt (Set [t]) -> Symbol nt t -> Set [t]
-- firstOne first_map (Terminal t) = Set.singleton [t]
-- firstOne first_map (Nonterminal nt) = first_map Map.! nt

truncatedProduct :: Ord t => Int -> Set [t] -> Set [t] -> Set [t]
truncatedProduct k a b = Set.fromList token_product
  where
    token_product = [take k $ ts ++ ts' | ts <- a_list, ts' <- b_list]
    a_list = Set.toList a
    b_list = Set.toList b

splitWhen :: (a -> Bool) -> [a] -> ([a], [a])
splitWhen = auxiliary []
  where
    auxiliary taken _ [] = (taken, [])
    auxiliary taken predicate not_taken@(x : xs)
      | predicate x = auxiliary (x : taken) predicate xs
      | otherwise = (taken, not_taken)

alphaBeta = auxiliary []
  where
    auxiliary taken [] = []
    auxiliary taken (x:xs) = (new_taken, xs) : auxiliary new_taken xs
      where
        new_taken = taken ++ [x]

firstOne' first_map wi = new_set
  where
    new_set
      | null wi = Set.singleton []
      | isTerminal a = terminal_set
      | isNonterminal a = nonterminal_set
      where
        a = head wi
        w' = tail wi
        terminal_set = Set.singleton [(\(Terminal x) -> x) a]
        nonterminal_set
          | any null first_set = not_null_set `Set.union` firstOne' first_map w'
          | otherwise = first_set
          where
            not_null_set = Set.filter (not . null) first_set
            nt = (\(Nonterminal n) -> n) a
            first_set = first_map Map.! nt

firstsOne' :: (Ord nt, Ord t) => Grammar nt t -> Map nt (Set [t])
firstsOne' grammar = fixedPointIterate (/=) f init_first_map
  where
    init_first_map = Map.fromList . map (,Set.empty) $ nonterminals grammar
    f first_map = Map.unionsWith Set.union $ map (auxiliary first_map) (productions grammar)
    auxiliary first_map (Production ai wi) = Map.adjust (Set.union new_set) ai first_map
      where
        new_set = firstOne' first_map wi

firstOne :: (Ord nt, Ord t) => Grammar nt t -> [Symbol nt t] -> Set [t]
firstOne grammar = firstOne' first_map
  where
    first_map = firstsOne' grammar

first' k first_map wi = new_set
  where
    new_set
      | null wi = Set.singleton []
      | isTerminal a = truncatedProduct k terminal_set (first' k first_map w')
      | isNonterminal a = truncatedProduct k nonterminal_set (first' k nt_first_map w')
      where
        a = head wi
        w' = tail wi
        Nonterminal nt = a
        nt_first_map = Map.adjust (Set.union nonterminal_set) nt first_map
        terminal_set = Set.singleton [(\(Terminal x) -> x) a]
        nonterminal_set
          | any null first_set = not_null_set `Set.union` first' k first_map w'
          | otherwise = first_set
          where
            not_null_set = Set.filter (not . null) first_set
            first_set = first_map Map.! nt

firsts :: (Ord nt, Ord t) => Int -> Grammar nt t -> Map nt (Set [t])
firsts k grammar = fixedPointIterate (/=) f init_first_map
  where
    init_first_map = Map.fromList . map (,Set.empty) $ nonterminals grammar
    f first_map = Map.unionsWith Set.union $ map (auxiliary first_map) (productions grammar)
    auxiliary first_map (Production ai wi) = Map.adjust (Set.union new_set) ai first_map
      where
        new_set = first' k first_map wi

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore _ [] = []
takeWhileOneMore predicate (x:xs)
  | predicate x = x : takeWhileOneMore predicate xs
  | otherwise = [x]

first :: (Ord nt, Ord t) => Int -> Grammar nt t -> [Symbol nt t] -> Set [t]
first k grammar = first' k first_map
  where
    first_map = firsts k grammar

-- first :: (Ord nt, Ord t) => Int -> Grammar nt t -> [Symbol nt t] -> Set [t]
-- first k grammar = Set.filter (not . null) . firstAB k grammar first_map
--   where
--     first_map = Set.filter (not . null) <$> firsts k grammar

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

constraints :: (Ord nt, Ord t) => Int -> Grammar nt t -> [Set (Constraint nt t)]
constraints k grammar = helper <$> productions grammar
  where
    nullable' = nullable grammar
    first' = first k grammar
    helper (Production nt s) = Set.unions $ uncurry auxiliary <$> right_symbols
      where
        right_symbols = rightSymbols s
        auxiliary nt' right = tConstraint `Set.union` ntConstraint
          where
            first_set = first' right
            right_nullable = nullable' right
            unempty = not . Set.null
            tConstraint =
              Set.fromList [TConstraint first_set nt' | unempty first_set]
            ntConstraint =
              Set.fromList [NTConstraint nt nt' | nt /= nt' && right_nullable]

-- follows k grammar = debug $ unextend $ fixedPointIterate (/=) f init_follow_map
--   where
--     unextend =
--       Map.mapKeys unextendNT
--         . fmap (Set.map (fmap unextendT . filter (/= End)) . Set.filter (not . all (== End)))
--         . Map.filterWithKey (\k a -> k /= start' || any (notElem End) a)
--     start' = start extended_grammar
--     (extended_grammar, stopper) = extendGrammar k grammar
--     first' = first k extended_grammar
--     [Production _ s] = findProductions extended_grammar (start extended_grammar)
--     nonterminals' = nonterminals extended_grammar
--     init_follow_map = Map.insert (start extended_grammar) (Set.singleton stopper) . Map.fromList . map (,Set.empty) $ nonterminals'
--     f = flip (foldl auxiliary) (productions extended_grammar)
--     auxiliary follow_map (Production aj symbols') = foldl helper follow_map right_symbols
--       where
--         right_symbols = rightSymbols symbols'
--         helper follow_map' (ai, w') = Map.adjust (Set.union new_set) ai follow_map'
--           where
--             new_set =
--               Set.unions
--                 [ first_set,
--                   follow_epsilon,
--                   follow_w',
--                   follow_prod
--                 ]
--             first_set = first' w'
--             follow_epsilon = if [] `elem` first_set then follow_map' Map.! aj else Set.empty
--             follow_w' = if null w' then follow_map' Map.! aj else Set.empty
--             follow_prod = truncatedProduct k first_set (follow_map' Map.! aj)

follows :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Map nt (Set [t])
follows k grammar = fixedPointIterate (/=) f init_follow_map
  where
    first' = first k grammar
    [Production _ s] = findProductions grammar (start grammar)
    stopper = Set.singleton . fmap (\(Terminal a) -> a) . reverse . takeWhile isTerminal $ reverse s
    init_follow_map = Map.insert (start grammar) stopper . Map.fromList . map (,Set.empty) $ nonterminals grammar
    f follow_map = Map.unionsWith Set.union $ map (auxiliary follow_map) all_right_productions
    all_right_productions = concatMap rightProductons $ productions grammar
    rightProductons (Production aj symbols') = (aj,) <$> rightSymbols symbols'
    auxiliary follow_map' (aj, (ai, w')) = Map.adjust (Set.union follow_prod) ai current_follow_map
      where
        sub_set =
          Set.unions
            [ first_set,
              follow_epsilon,
              follow_w'
            ]
        first_set = first' w'
        current_follow_map = Map.adjust (Set.union sub_set) ai follow_map'
        follow_epsilon = if [] `elem` first_set then follow_map' Map.! aj else Set.empty
        follow_w' = if null w' then follow_map' Map.! aj else Set.empty
        follow_prod = truncatedProduct k first_set (current_follow_map Map.! aj)

-- follows k grammar = unextend $ fixedPointIterate (/=) f init_follow_map
--   where
--     unextend =
--       Map.mapKeys unextendNT
--         . fmap (Set.map (fmap unextendT . filter (/= End)) . Set.filter (not . all (== End)))
--         . Map.filterWithKey (\k a -> k /= start')
--     start' = start extended_grammar
--     (extended_grammar, stopper) = extendGrammar k grammar
--     first' = first k extended_grammar
--     [Production _ s] = findProductions extended_grammar (start extended_grammar)
--     nonterminals' = nonterminals extended_grammar
--     init_follow_map = Map.insert (start extended_grammar) (Set.singleton stopper) . Map.fromList . map (,Set.empty) $ nonterminals'
--     f = flip (foldl auxiliary) (productions extended_grammar)
--     auxiliary follow_map (Production aj symbols') = foldl helper follow_map right_symbols
--       where
--         right_symbols = rightSymbols symbols'
--         helper follow_map' (ai, w') = Map.adjust (Set.union new_set) ai follow_map'
--           where
--             new_set =
--               Set.unions
--                 [ first_set,
--                   follow_epsilon,
--                   follow_w',
--                   follow_prod
--                 ]
--             first_set = first' w'
--             follow_epsilon = if [] `elem` first_set then follow_map' Map.! aj else Set.empty
--             follow_w' = if null w' then follow_map' Map.! aj else Set.empty
--             follow_prod = truncatedProduct k first_set (follow_map' Map.! aj)

-- follows :: (Ord nt, Ord t) => Int -> Grammar nt t -> Map nt (Set [t])
-- follows k grammar = fixedPointIterate (/=) f init_follow_map
--   where
--     constraints' = Set.unions $ constraints k grammar
--     init_follow_map = Map.fromList . map (,Set.singleton []) $ nonterminals grammar
--     f a = foldl addConstraint a constraints'
--     addConstraint m (TConstraint t nt) = Map.adjust (Set.union t) nt m
--     addConstraint m (NTConstraint nt nt') = Map.adjust (truncatedProduct k res) nt' m
--       where
--         res = m Map.! nt

follow :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> nt -> Set [t]
follow k grammar = (follows' Map.!)
  where
    follows' = follows k grammar

last :: (Ord nt, Ord t) => Int -> Grammar nt t -> [Symbol nt t] -> Set [t]
last q grammar = Set.map reverse . lasts . reverse
  where
    lasts = first q $ reverseGrammar grammar

before :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> nt -> Set [t]
before q grammar = Set.map reverse . (befores Map.!)
  where
    befores = follows q $ reverseGrammar grammar

firstTable :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Map (nt, [t]) Int
firstTable k grammar = Map.unions $ auxiliary <$> zip prods [0 ..]
  where
    prods = productions grammar
    first' = first k grammar
    auxiliary (Production nt a, i) = Map.fromList [((nt, y), i) | y <- ts]
      where
        ts = Set.toList $ first' a

nullableFollowTable ::
  (Ord nt, Ord t, Show nt, Show t) =>
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

-- llTable :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Map (nt, [t]) Int
-- llTable k grammar = Map.union first_table nullable_follow_table
--   where
--     first_table = firstTable k grammar
--     nullable_follow_table = nullableFollowTable k grammar

llTable :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Map (nt, [t]) Int
llTable k grammar = Map.union first_table follow_table
  where
    first_table = Map.unions $ firstEntry <$> zip [0 ..] prods
    follow_table = Map.unions $ followEntry <$> zip [0 ..] prods
    prods = productions grammar
    first' = first k grammar
    firstEntry (i, Production nt a) = Map.fromList [((nt, y), i) | y <- ts]
      where
        ts = Set.toList $ first' a
    follow' = follow k grammar
    followEntry (i, Production nt a) =
      Map.fromList [((nt, y), i) | is_nullable, y <- nts]
      where
        nts = Set.toList $ follow' nt
        is_nullable = [] `Set.member` first' a

llParse ::
  (Ord nt, Ord t, Show nt, Show t) =>
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
      | null keys = Nothing
      | isNothing maybeTuple = Nothing
      | otherwise = auxiliary (input, production ++ ys, index : parsed)
      where
        keys =
          filter (`Map.member` table)
            . fmap (y,)
            . takeWhile (not . null)
            . iterate init
            $ take k input

        maybeTuple = do
          index <- List.last keys `Map.lookup` table
          production <- index `Map.lookup` production_map
          return (index, symbols production)

        Just (index, production) = maybeTuple
