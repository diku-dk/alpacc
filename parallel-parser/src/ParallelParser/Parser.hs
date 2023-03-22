module ParallelParser.Parser
  ( nullable,
    first,
    follow,
    last,
    before,
    Item (..),
    llpCollection,
    DotProduction (..),
    toDotProduction,
    llkParse,
    psls,
    llpParsingTable,
    llpParse,
    futharkKeyGeneration,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Composition
import Data.Foldable
import Data.Function (flip, on, ($), (.))
import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Map (Map (..))
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq (..), (<|), (><), (|>))
import qualified Data.Sequence as SQ
import Data.Set (Set (..))
import qualified Data.Set as S
import Debug.Trace (traceShow)
import ParallelParser.Grammar
import Data.String.Interpolate ( i )
import Prelude hiding (last)
import Control.Arrow (Arrow(arr))

debug x = traceShow ("DEBUG: " ++ show x) x

checkpoint = traceShow "Checkpoint"

fixedPointIterate :: Eq b => (b -> b -> Bool) -> (b -> b) -> b -> b
fixedPointIterate cmp f a = fst . head . dropWhile (uncurry cmp) . drop 1 $ iterate swapApply (a, a)
  where
    swapApply (n, _) = (f n, n)

nullables :: (Ord nt, Ord t) => Grammar nt t -> Map nt Bool
nullables grammar = fixedPointIterate (/=) (nullableNontermianl productions_map) init_nullable_map
  where
    init_nullable_map = M.fromList . map (,False) $ nonterminals grammar
    nullable' _ (Terminal _) = False
    nullable' nullable_map (Nonterminal a) = nullable_map M.! a
    productions_map = toProductionsMap $ productions grammar
    nullableNontermianl prods nullable_map = (any . all $ nullable' nullable_map) <$> prods

nullableOne :: (Ord nt, Ord t) => Grammar nt t -> Symbol nt t -> Bool
nullableOne _ (Terminal t) = False
nullableOne grammar (Nonterminal nt) = nullable_map M.! nt
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

firstOne :: Ord nt => Map nt (Set [t]) -> Symbol nt t -> Set [t]
firstOne first_map (Terminal t) = S.singleton [t]
firstOne first_map (Nonterminal nt) = first_map M.! nt

truncatedProduct :: Ord t => Int -> Set [t] -> Set [t] -> Set [t]
truncatedProduct k a b = S.fromList [take k $ ts ++ ts' | ts <- a_list, ts' <- b_list]
  where
    a_list = S.toList a
    b_list = S.toList b

firstAB ::
  (Ord nt, Ord t) =>
  Int ->
  Grammar nt t ->
  Map nt (Set [t]) ->
  [Symbol nt t] ->
  Set [t]
firstAB k grammar first_map = foldl truncatedProductSymbol (S.singleton []) . kNullables
  where
    truncatedProductSymbol a b = truncatedProduct k a (firstOne first_map b)
    kNullables = takeWhileNMore k nullableOne'
    nullableOne' = nullableOne grammar

firsts :: (Ord nt, Ord t) => Int -> Grammar nt t -> Map nt (Set [t])
firsts k grammar = fixedPointIterate (/=) (firstNontermianl productions_map) init_first_map
  where
    firstAB' = firstAB k grammar
    init_first_map = M.fromList . map (,S.singleton []) $ nonterminals grammar
    productions_map = toProductionsMap $ productions grammar
    firstNontermianl prods first_map = fmap (S.unions . fmap (firstAB' first_map)) prods

first :: (Ord nt, Ord t) => Int -> Grammar nt t -> [Symbol nt t] -> Set [t]
first k grammar = S.filter (not . null) . firstAB k grammar first_map
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
  show (TConstraint ts nt) = "{" ++ L.intercalate ", " (show <$> S.toList ts) ++ "} ⊆ " ++ show nt
  show (NTConstraint nt nt') = show nt ++ " ⊆ " ++ show nt'

constraints :: (Ord nt, Ord t) => Int -> Grammar nt t -> [Set (Constraint nt t)]
constraints k grammar = helper <$> productions grammar
  where
    nullable' = nullable grammar
    first' = first k grammar
    helper (Production nt s) = S.unions $ uncurry auxiliary <$> right_symbols
      where
        right_symbols = rightSymbols s
        auxiliary nt' right = tConstraint `S.union` ntConstraint
          where
            first_set = first' right
            tConstraint = S.fromList [TConstraint first_set nt' | not (S.null first_set)]
            ntConstraint = S.fromList [NTConstraint nt nt' | nt /= nt' && nullable' right]

follows :: (Ord nt, Ord t) => Int -> Grammar nt t -> Map nt (Set [t])
follows k grammar = fixedPointIterate (/=) f init_follow_map
  where
    constraints' = S.unions $ constraints k grammar
    init_follow_map = M.fromList . map (,S.empty) $ nonterminals grammar
    addConstraint (TConstraint t nt) m = M.adjust (`S.union` t) nt m
    addConstraint (NTConstraint nt nt') m = M.adjust (`S.union` (m M.! nt)) nt' m
    f a = foldl (flip addConstraint) a constraints'

follow :: (Ord nt, Ord t) => Int -> Grammar nt t -> nt -> Set [t]
follow k grammar = (follows' M.!)
  where
    follows' = follows k grammar

last :: (Ord nt, Ord t) => Int -> Grammar nt t -> [Symbol nt t] -> Set [t]
last q grammar = S.map reverse . lasts . reverse
  where
    lasts = first q $ reverseGrammar grammar

before :: (Ord nt, Ord t) => Int -> Grammar nt t -> nt -> Set [t]
before q grammar = S.map reverse . (befores M.!)
  where
    befores = follows q $ reverseGrammar grammar

firstTable :: (Ord nt, Ord t) => Int -> Grammar nt t -> Map (nt, [t]) Int
firstTable k grammar = M.unions $ auxiliary <$> zip (productions grammar) [0 ..]
  where
    first' = first k grammar
    auxiliary (Production nt a, i) = M.fromList [((nt, y), i) | y <- S.toList $ first' a]

nullableFollowTable :: (Ord nt, Ord t) => Int -> Grammar nt t -> Map (nt, [t]) Int
nullableFollowTable k grammar = M.unions $ auxiliary <$> zip [0 ..] (productions grammar)
  where
    follow' = follow k grammar
    nullable' = nullable grammar
    auxiliary (i, Production nt a) = M.fromList [((nt, y), i) | nullable' a, y <- S.toList $ follow' nt]

llkTable :: (Ord nt, Ord t) => Int -> Grammar nt t -> Map (nt, [t]) Int
llkTable k grammar = M.union first_table nullable_follow_table
  where
    first_table = firstTable k grammar
    nullable_follow_table = nullableFollowTable k grammar

llkParse :: (Ord nt, Ord t) => Int -> Grammar nt t -> ([t], [Symbol nt t], [Int]) -> Maybe ([t], [Symbol nt t], [Int])
llkParse k grammar = auxiliary
  where
    table = llkTable k grammar
    production_map = M.fromList . zip [0 ..] $ productions grammar
    auxiliary ([], stack, parsed) = Just ([], stack, reverse parsed)
    auxiliary (input, [], parsed) = Just (input, [], reverse parsed)
    auxiliary (x : xs, (Terminal y) : ys, parsed)
      | x == y = auxiliary (xs, ys, parsed)
      | otherwise = Nothing
    auxiliary (input, (Nonterminal y) : ys, parsed)
      | L.null keys = Nothing
      | isNothing maybeTuple = Nothing
      | otherwise = auxiliary (input, production ++ ys, index : parsed)
      where
        keys =
          L.filter (`M.member` table)
            . fmap (y,)
            . takeWhile (not . L.null)
            . iterate init
            $ take k input

        maybeTuple = do
          index <- L.last keys `M.lookup` table
          production <- index `M.lookup` production_map
          return (index, symbols production)

        Just (index, production) = maybeTuple

data DotProduction nt t = DotProduction nt [Symbol nt t] [Symbol nt t] deriving (Ord, Eq, Show, Read)

toDotProduction :: Production nt t -> DotProduction nt t
toDotProduction (Production nt s) = DotProduction nt s []

data Item nt t = Item
  { dotProduction :: DotProduction nt t,
    suffix :: [t],
    prefix :: [t],
    shortestPrefix :: [Symbol nt t]
  }
  deriving (Eq, Ord, Show)

initD :: (Ord nt, Ord t) => Int -> Grammar nt t -> S.Set (Item nt t)
initD q grammar
  | null last' = S.singleton (auxiliary [])
  | otherwise = auxiliary `S.map` last'
  where
    production' = head $ findProductions grammar (start grammar)
    last' = last q grammar $ symbols production'
    auxiliary x =
      Item
        { dotProduction = toDotProduction production',
          suffix = x,
          prefix = [],
          shortestPrefix = []
        }

expansions :: (Ord t, Ord nt) => Grammar nt t -> [Symbol nt t] -> Seq [Symbol nt t]
expansions grammar = expand SQ.empty SQ.empty . SQ.fromList
  where
    production_map = fmap (fmap SQ.fromList) . toProductionsMap $ productions grammar
    expand es _ SQ.Empty = es
    expand es ys ((Terminal x) :<| xs) = expand es (ys :|> Terminal x) xs
    expand es ys ((Nonterminal x) :<| xs) = expand (es >< ps) (ys :|> Nonterminal x) xs
      where
        smallExpand e = ys >< e >< xs
        ps = SQ.fromList $ toList . smallExpand <$> (production_map M.! x)

isTerminalPrefix :: (Ord t, Ord nt) => Grammar nt t -> t -> [Symbol nt t] -> Bool
isTerminalPrefix grammar t = bfs S.empty . SQ.singleton
  where
    expansions' = expansions grammar
    bfs _ SQ.Empty = False
    bfs visited (top :<| queue)
      | top `S.member` visited = bfs new_visited queue
      | null top = bfs new_visited queue
      | Terminal t == head top = True
      | isTerminal $ head top = bfs new_visited queue
      | otherwise = bfs new_visited (queue >< expansions' top)
      where
        new_visited = S.insert top visited

leftExpansions :: (Ord t, Ord nt) => Grammar nt t -> [Symbol nt t] -> SQ.Seq [Symbol nt t]
leftExpansions grammar = expand SQ.empty . SQ.fromList
  where
    production_map = fmap (fmap SQ.fromList) . toProductionsMap $ productions grammar
    expand _ SQ.Empty = SQ.Empty
    expand ys ((Terminal x) :<| xs) = expand (ys :|> Terminal x) xs
    expand ys ((Nonterminal x) :<| xs) = ps
      where
        smallExpand e = ys >< e >< xs
        ps = SQ.fromList $ toList . smallExpand <$> (production_map M.! x)

allStarts :: (Ord nt, Ord t) => Int -> Grammar nt t -> [[t]]
allStarts k grammar = bfs S.empty (SQ.singleton . L.singleton . toNonterminal $ start grammar)
  where
    toNonterminal :: nt -> Symbol nt t
    toNonterminal = Nonterminal 
    unpackT (Terminal t) = t
    expansions' = leftExpansions grammar
    bfs _ SQ.Empty = []
    bfs visited (top :<| queue)
      | top `S.member` visited = bfs new_visited queue
      | null top = bfs new_visited queue
      | all isTerminal k_terms = (unpackT <$> k_terms) : bfs new_visited queue
      | otherwise = bfs new_visited (queue >< expansions' top)
      where
        k_terms = take k top
        new_visited = S.insert top visited

solveShortestsPrefix :: (Ord t, Ord nt) => Grammar nt t -> t -> [Symbol nt t] -> [Symbol nt t]
solveShortestsPrefix grammar t = solveShortestsPrefix'
  where
    prefixes = reverse . takeWhile (not . L.null) . iterate init
    nullable' = nullable grammar
    isTerminalPrefix' = isTerminalPrefix grammar t
    solveShortestsPrefix' = safeHead . dropWhile (not . isTerminalPrefix') . prefixes
    safeHead [] = []
    safeHead (x : xs) = x

newLlpItems :: (Ord t, Ord nt) => Int -> Int -> Grammar nt t -> [t] -> [Symbol nt t] -> DotProduction nt t -> S.Set (Item nt t)
newLlpItems q k grammar vi delta dot_production = S.fromList [newItem u v | u <- uj, v <- vj]
  where
    uj = toList . S.unions $ (last q grammar . (++ alpha) . fmap Terminal) `S.map` S.insert [] (before q grammar y)
    vj = toList . S.unions $ (first k grammar . (x ++) . fmap Terminal) `S.map` S.fromList [[], vi]
    x = [L.head x_beta | not (L.null x_beta)]
    x_delta = x ++ delta
    solveShortestsPrefix' v = solveShortestsPrefix grammar v x_delta
    newItem u v =
      Item
        { dotProduction = dot_production,
          suffix = u,
          prefix = v,
          shortestPrefix = solveShortestsPrefix' $ head v
        }
    (DotProduction y alpha x_beta) = dot_production

addLlpItem :: (Ord t, Ord nt) => Int -> Grammar nt t -> p -> Item nt t -> Set (Item nt t)
addLlpItem q grammar items old_item
  | null alpha_y = S.empty
  | isTerminal $ L.last alpha_y = S.empty
  | otherwise = S.unions $ newItems <$> deltas
  where
    Item
      { dotProduction = DotProduction x alpha_y beta,
        suffix = u,
        prefix = v,
        shortestPrefix = gamma
      } = old_item
    y = (\(Nonterminal nt) -> nt) $ L.last alpha_y
    productions' = productions grammar
    deltas = symbols <$> findProductions grammar y
    newItems delta = S.fromList [newItem y | y <- u']
      where
        u' = toList . S.unions $ (last q grammar . (++ delta) . fmap Terminal) `S.map` S.insert [] (before q grammar y)
        newItem z =
          Item
            { dotProduction = DotProduction y delta [],
              suffix = z,
              prefix = v,
              shortestPrefix = gamma
            }

addLlpItems :: (Ord t, Ord nt) => Int -> Grammar nt t -> Set (Item nt t) -> Set (Item nt t)
addLlpItems q grammar items = S.union items . S.unions $ addLlpItem q grammar items `S.map` items

moveDot :: DotProduction nt t -> Maybe (DotProduction nt t)
moveDot (DotProduction _ [] _) = Nothing
moveDot (DotProduction nt s s') = Just $ DotProduction nt (init s) (L.last s : s')

nextLlpItems :: (Ord t, Ord nt) => Int -> Int -> Grammar nt t -> Item nt t -> Maybe (S.Set (Item nt t))
nextLlpItems q k grammar llp_item = newLlpItem' <$> moveDot dot_production
  where
    newLlpItem' = newLlpItems q k grammar v delta
    dot_production = dotProduction llp_item
    v = prefix llp_item
    delta = shortestPrefix llp_item

solveLlpItems :: (Ord t, Ord nt) => Int -> Int -> Grammar nt t -> Set (Item nt t) -> Set (Set (Item nt t))
solveLlpItems q k grammar =
  S.map (fixedPointIterate (/=) (addLlpItems q grammar))
    . S.fromList
    . fmap (S.unions . mapMaybe (nextLlpItems q k grammar))
    . L.groupBy ((==) `on` auxiliary)
    . L.sortOn auxiliary
    . toList
  where
    auxiliary = safeLast . (\(DotProduction _ xs _) -> xs) . dotProduction
    safeLast [] = Nothing
    safeLast x = Just $ L.last x

llpCollection :: (Ord t, Ord nt) => Int -> Int -> Grammar (AugmentedNonterminal nt) (AugmentedTerminal t) -> Set (Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)))
llpCollection q k grammar = S.filter (not . null) $ auxiliary S.empty d_init (toList d_init)
  where
    d_init = S.singleton $ initD q grammar
    solveLlpItems' = solveLlpItems q k grammar
    auxiliary _ items [] = items
    auxiliary visited items (x : xs)
      | x `S.member` visited = auxiliary visited items xs
      | otherwise = auxiliary new_visited new_items new_queue
      where
        new_visited = S.insert x visited
        new_item = solveLlpItems' x
        new_items = S.union new_item items
        new_queue = xs ++ toList new_item

psls :: (Ord t, Ord nt) => Set (Set (Item nt t)) -> Map ([t], [t]) (Set [Symbol nt t])
psls = M.unionsWith S.union . fmap auxiliary . toList . S.unions
  where
    auxiliary old_item
      | null gamma = M.empty
      | null alpha_z = M.empty
      | isTerminal z = M.singleton (x, y) (S.singleton gamma)
      | otherwise = M.empty
      where
        Item
          { dotProduction = DotProduction _ alpha_z _,
            suffix = x,
            prefix = y,
            shortestPrefix = gamma
          } = old_item
        z = L.last alpha_z

llkParse' :: (Ord nt, Ord t) => Int -> Grammar nt t -> [t] -> [Symbol nt t] -> Maybe ([t], [Symbol nt t], [Int])
llkParse' k grammar a b = auxiliary (a, b, [])
  where
    table = llkTable k grammar
    production_map = M.fromList . zip [0 ..] $ productions grammar
    auxiliary ([], stack, parsed) = Just ([], stack, reverse parsed)
    auxiliary (input, [], parsed) = Just (input, [], reverse parsed)
    auxiliary (x : xs, (Terminal y) : ys, parsed)
      | x == y = Just ([], ys, reverse parsed)
      | otherwise = Nothing
    auxiliary (input, (Nonterminal y) : ys, parsed)
      | L.null keys = Nothing
      | isNothing maybeTuple = Nothing
      | otherwise = auxiliary (input, production ++ ys, index : parsed)
      where
        keys =
          L.filter (`M.member` table)
            . fmap (y,)
            . takeWhile (not . L.null)
            . iterate init
            $ take k input

        maybeTuple = do
          index <- L.last keys `M.lookup` table
          production <- index `M.lookup` production_map
          return (index, symbols production)

        Just (index, production) = maybeTuple

llpParsingTable ::
  (Ord nt, Ord t) =>
  Int ->
  Int ->
  Grammar nt t ->
  Map ([AugmentedTerminal t], [AugmentedTerminal t]) [Int]
llpParsingTable q k grammar = M.union starts $ fromJust <$> M.mapWithKey auxiliary (S.findMax <$> psls_table)
  where
    augmented_grammar = augmentGrammar grammar
    starts = M.fromList . map ((,[0]) . ([],)) $ allStarts k augmented_grammar
    collection = llpCollection q k augmented_grammar
    psls_table = psls collection
    auxiliary (x, y) alpha = f <$> llkParse' k augmented_grammar y alpha
      where
        f (epsilon, omega, pi) = pi

substrings :: Int -> Int -> [a] -> [([a], [a])]
substrings q k = toList . auxiliary SQ.empty SQ.empty . SQ.fromList
  where
    auxiliary es _ SQ.Empty = es
    auxiliary es ys (x :<| xs) = auxiliary (es :|> substring) (ys :|> x) xs
      where
        backwards = takeR q ys
        forwards = SQ.take (k - 1) xs
        substring = (toList backwards, toList $ x <| forwards)
        takeR i seq = SQ.drop (n - i) seq
          where
            n = SQ.length seq

llpParse :: (Ord nt, Ord t) => Int -> Int -> Grammar nt t -> [t] -> [Int]
llpParse q k grammar = concatMap auxiliary . substrings q k . (RightTurnstile :) . (++ [LeftTurnstile]) . aug
  where
    aug = fmap AugmentedTerminal
    (Just i) = L.findIndex (\(Production nt _) -> nt == start grammar) (productions grammar)
    table = llpParsingTable q k grammar
    auxiliary ([], RightTurnstile : _) = [0]
    auxiliary (back, forw) = p
      where
        p = L.head $ mapMaybe (`M.lookup` table) [(a, b) | a <- helper back, b <- helper forw]
        helper = takeWhile (not . null) . iterate init

lpad p m xs = replicate (m - length ys) p ++ ys
  where ys = take m xs

rpad p m xs = ys ++ replicate (m - length ys) p
  where ys = take m xs

futharkKeyGeneration :: (Ord nt, Ord t) => Int -> Int -> Grammar nt t -> String
futharkKeyGeneration q k grammar =
  [i|
def lookbkack_array_to_tuple [n] (arr : [n]u32) =
  #{toTupleArray "arr" q}

def lookahead_array_to_tuple [n] (arr : [n]u32) =
  #{toTupleArray "arr" k}

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
  #{cases_printed}
  case _ -> #{last_case}

def parse [n] (arr : [n]u32) =
  let arr' = [0] ++ arr ++ [1]
  in keys arr'
  |> map key_to_productions
  |> flatten
  |> filter (!=u32.highest)
|]
  where
    toTuple = ('(':) . (++")") . L.intercalate ", " 
    toTupleArray name n = toTuple $ map (((name ++ "[") ++) . (++"]") . show) [0..n - 1]
    lookback_type = toTuple $ replicate q "u32"
    lookahead_type = toTuple $ replicate k "u32"
    table = llpParsingTable q k grammar
    max_size = maximum $ length <$> table
    augmented_grammar = augmentGrammar grammar
    backPad = lpad "4294967295" q
    frontPad = rpad "4294967295" k
    terminal_map = M.fromList . flip zip (show <$> [0..]) $ terminals augmented_grammar
    convert = map (terminal_map M.!)
    padded_table = ('[':) . (++"]") . L.intercalate ", " . lpad "u32.highest" max_size . map show <$> M.mapKeys (\(a, b) -> (backPad $ convert a, frontPad $ convert b)) table
    cases_printed = L.intercalate "\n  " $ (\(k, v) -> "case " ++ printKey k ++ " -> " ++ v) <$> M.toList padded_table
    printKey (b, f) = [i|((#{L.intercalate ", " b}), (#{L.intercalate ", " f}))|]
    last_case = (++"]") . ('[':) . L.intercalate ", " $ replicate max_size "u32.highest"