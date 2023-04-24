module ParallelParser.LL
  ( nullable,
    first,
    follow,
    last,
    before,
    fixedPointIterate,
    llTable,
    llParse,
    leftDerivations,
    derivations,
    naiveFirst,
    naiveFollow,
    alphaBeta
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
import Data.Sequence (Seq (..), (<|), (><), (|>))
import qualified Data.Sequence as Seq hiding (Seq (..), (<|), (><), (|>))
import Data.Foldable
import Data.Bifunctor (Bifunctor (second))
import Data.Composition
import Debug.Trace (traceShow)

debug x = traceShow x x

rightProductons :: Production nt t -> [(nt, (nt, [Symbol nt t]))]
rightProductons (Production aj symbols') = (aj,) <$> rightSymbols symbols'

leftDerivations ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Grammar nt t ->
  [Symbol nt t] ->
  Seq [Symbol nt t]
leftDerivations grammar = derive Seq.empty . Seq.fromList
  where
    toSequences = fmap (fmap Seq.fromList)
    production_map = toSequences . toProductionsMap $ productions grammar
    derive _ Empty = Empty
    derive ys ((Terminal x) :<| xs) = derive (ys :|> Terminal x) xs
    derive ys ((Nonterminal x) :<| xs) = ps
      where
        smallDerive e = ys >< e >< xs
        ps = Seq.fromList $ toList . smallDerive <$> (production_map Map.! x)

derivations ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Grammar nt t ->
  [Symbol nt t] ->
  Seq [Symbol nt t]
derivations grammar = derive Seq.empty Seq.empty . Seq.fromList
  where
    toSeq = fmap (fmap Seq.fromList)
    production_map = toSeq . toProductionsMap $ productions grammar
    derive es _ Empty = es
    derive es ys ((Terminal x) :<| xs) = derive es (ys :|> Terminal x) xs
    derive es ys ((Nonterminal x) :<| xs) = derive es' (ys :|> Nonterminal x) xs
      where
        es' = es >< ps
        smallDerive e = ys >< e >< xs
        ps = Seq.fromList $ toList . smallDerive <$> (production_map Map.! x)

naiveFirst :: (Show nt, Show t, Ord nt, Ord t) => Int -> Grammar nt t -> [Symbol nt t] -> Set [t]
naiveFirst k grammar = Set.fromList . bfs Set.empty . Seq.singleton
  where
    unpackT (Terminal t) = t
    leftDerivations' = leftDerivations grammar
    bfs _ Empty = []
    bfs visited (top :<| queue)
      | k_terms `Set.member` visited = bfs visited queue
      | all isTerminal k_terms = (unpackT <$> k_terms) : bfs new_visited (queue >< leftDerivations' top)
      | otherwise = bfs new_visited (queue >< leftDerivations' top)
      where
        k_terms = take k top
        new_visited = Set.insert k_terms visited

naiveFollows :: (Show nt, Show t, Ord nt, Ord t) => Int -> Grammar nt t -> Map nt (Set [t])
naiveFollows k grammar =
  Map.unionWith Set.union init_maps
    . Map.unionsWith Set.union
    $ bfs Set.empty start'
  where
    [Production nt s] = findProductions grammar (start grammar)
    init_maps = Map.fromList $ (,Set.empty) <$> nonterminals grammar
    first' = first k grammar
    start' = Seq.singleton s
    unpackT (Terminal t) = t
    derivations' = derivations grammar
    bfs _ Empty = []
    bfs visited (top :<| queue)
      | right_symbols_set `Set.isSubsetOf` visited = bfs visited queue
      | null right_symbols = bfs new_visited queue
      | otherwise = maps ++ bfs new_visited (queue >< derivations' top)
      where
        right_symbols = second first' <$> rightSymbols top
        maps = uncurry Map.singleton <$> right_symbols
        right_symbols_set = Set.fromList right_symbols
        new_visited = right_symbols_set `Set.union` visited

naiveFollow :: (Show nt, Show t, Ord nt, Ord t) => Int -> Grammar nt t -> nt -> Set [t]
naiveFollow k grammar = (follow_map Map.!)
  where
    follow_map = naiveFollows k grammar

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

truncatedProduct :: Ord t => Int -> Set [t] -> Set [t] -> Set [t]
truncatedProduct k a b = Set.fromList token_product
  where
    token_product = [take k $ ts ++ ts' | ts <- a_list, ts' <- b_list]
    a_list = Set.toList a
    b_list = Set.toList b

alphaBeta :: [a] -> [([a], [a])]
alphaBeta string
  | null string = []
  | otherwise = auxiliary [] string
  where
    auxiliary taken [x] = []
    auxiliary taken (x:xs) = (new_taken, xs) : auxiliary new_taken xs
      where
        new_taken = taken ++ [x]

alphaBetaProducts :: (Ord nt, Ord t) => Int -> Map nt (Set [t]) -> [Symbol nt t] -> Set [t]
alphaBetaProducts _ _ [] = error "Input string cannot be empty."
alphaBetaProducts k first_map [Terminal t] = Set.singleton [t]
alphaBetaProducts k first_map [Nonterminal nt] = first_map Map.! nt
alphaBetaProducts k first_map string = Set.unions $ map subProducts alpha_betas
  where
    alpha_betas = alphaBeta string
    subProduct = alphaBetaProducts k first_map
    subProducts = uncurry (truncatedProduct k) . both subProduct

first' :: (Ord nt, Ord t) => Int -> Map nt (Set [t]) -> [Symbol nt t] -> Set [t]
first' k first_map wi = new_set
  where
    new_set
      | null wi = Set.singleton []
      | otherwise = alphaBetaProducts k first_map wi
      where
        alpha_betas = alphaBeta wi

firsts :: (Ord nt, Ord t) => Int -> Grammar nt t -> Map nt (Set [t])
firsts k grammar = fixedPointIterate (/=) f init_first_map
  where
    init_first_map = Map.fromList . map (,Set.empty) $ nonterminals grammar
    f first_map = Map.unionsWith Set.union $ map (auxiliary first_map) (productions grammar)
    auxiliary first_map (Production ai wi) = Map.adjust (Set.union new_set) ai first_map
      where
        new_set = first' k first_map wi

first :: (Show nt, Show t, Ord nt, Ord t) => Int -> Grammar nt t -> [Symbol nt t] -> Set [t]
first k grammar = first' k first_map
  where
    first_map = firsts k grammar

rightSymbols :: [Symbol nt t] -> [(nt, [Symbol nt t])]
rightSymbols [] = []
rightSymbols ((Terminal _) : xs) = rightSymbols xs
rightSymbols ((Nonterminal x) : xs) = (x, xs) : rightSymbols xs

dropWhileMinusOne :: (a -> Bool) -> [a] -> [a]
dropWhileMinusOne = auxiliary Nothing
  where
    auxiliary Nothing _ [] = []
    auxiliary (Just last) _ [] = [last]
    auxiliary last predicate (x:xs)
      | predicate x = auxiliary (Just x) predicate xs
      | isJust last = fromJust last:x:xs
      | otherwise = x:xs

followsOne :: (Ord nt, Ord t, Show nt, Show t) => Grammar nt t -> Maybe t -> Map nt (Set [t])
followsOne grammar end = fixedPointIterate (/=) f init_follow_map
  where
    first' = first 1 grammar
    stopper = Set.singleton . concat . maybeToList $ List.singleton <$> end
    init_follow_map = Map.insert (start grammar) stopper . Map.fromList . map (,Set.empty) $ nonterminals grammar
    f follow_map = Map.unionsWith Set.union $ map (auxiliary follow_map) right_productions
    right_productions = concatMap rightProductons $ productions grammar
    auxiliary follow_map' (aj, (ai, w')) = Map.adjust (Set.union new_set) ai follow_map'
      where
        new_set =
          Set.unions
            [ first_set,
              follow_epsilon,
              follow_w'
            ]
        first_set = first' w'
        follow_epsilon = if [] `elem` first_set then follow_map' Map.! aj else Set.empty
        follow_w' = if null w' then follow_map' Map.! aj else Set.empty

follows :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Maybe t -> Map nt (Set [t])
follows k grammar end = fixedPointIterate (/=) f init_follow_map
  where
    first' = first k grammar
    [Production nt syms] = findProductions grammar (start grammar)
    Just (Nonterminal s) = find isNonterminal syms
    stopper = Set.singleton . concat . maybeToList $ replicate k <$> end
    init_follow_map = Map.insert s stopper . Map.fromList . map (,Set.empty) $ nonterminals grammar
    f follow_map = Map.unionsWith Set.union $ map (auxiliary follow_map) right_productions
    right_productions = concatMap rightProductons $ productions grammar
    auxiliary follow_map' (aj, (ai, w')) = Map.adjust (Set.union subset) ai follow_map'
      where
        first_set = first' w'
        subset = truncatedProduct k first_set (follow_map' Map.! aj)

follow :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> nt -> Set [t]
follow k grammar = (follows' Map.!)
  where
    follows' = follows k grammar (rightPadding grammar)

last :: (Show nt, Show a, Ord nt, Ord a) => Int -> Grammar nt a -> [Symbol nt a] -> Set [a]
last q grammar = Set.map reverse . lasts . reverse
  where
    lasts = naiveFirst q $ reverseGrammar grammar

before :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> nt -> Set [t]
before q grammar = Set.map reverse . (befores Map.!)
  where
    befores = follows q (reverseGrammar grammar) (leftPadding grammar)

llTable :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Map (nt, [t]) Int
llTable k grammar = Map.union first_table follow_table
  where
    first_table = Map.unions $ zipWith firstEntry [0 ..] prods
    follow_table = Map.unions $ zipWith followEntry [0 ..] prods
    prods = productions grammar
    first' = first k grammar
    firstEntry i (Production nt a) = Map.fromList [((nt, y), i) | y <- ts]
      where
        ts = Set.toList $ first' a
    follow' = follow k grammar
    followEntry i (Production nt a) =
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
          index <- head keys `Map.lookup` table
          production <- index `Map.lookup` production_map
          return (index, symbols production)

        Just (index, production) = maybeTuple
