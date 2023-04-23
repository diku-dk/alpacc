module ParallelParser.LLP
  ( Item (..),
    llpCollection,
    DotProduction (..),
    toDotProduction,
    psls,
    llpParsingTable,
    llpParse,
    leftRecursiveNonterminals,
    llTableParse
  )
where

import Data.Foldable
import Data.Function (flip, on, ($), (.))
import qualified Data.List as List
import Data.Map (Map (..))
import qualified Data.Map as Map hiding (Map (..))
import Data.Maybe
import Data.Sequence (Seq (..), (<|), (><), (|>))
import qualified Data.Sequence as Seq hiding (Seq (..), (<|), (><), (|>))
import Data.Set (Set (..))
import qualified Data.Set as Set hiding (Set (..))
import Debug.Trace (traceShow)
import ParallelParser.Grammar
import ParallelParser.LL
import Prelude hiding (last)
import Control.Parallel.Strategies

pmap :: NFData b => (a -> b) -> [a] -> [b]
pmap f ls =
        let bs = map f ls
            cs = bs `using` parList rdeepseq
        in cs

debug x = traceShow x x

data DotProduction nt t
  = DotProduction nt [Symbol nt t] [Symbol nt t]
  deriving (Ord, Eq, Show, Read)

toDotProduction :: Production nt t -> DotProduction nt t
toDotProduction (Production nt s) = DotProduction nt s []

data Item nt t = Item
  { dotProduction :: DotProduction nt t,
    suffix :: [t],
    prefix :: [t],
    shortestPrefix :: [Symbol nt t]
  }
  deriving (Eq, Ord, Show)

initD :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Set (Item nt t)
initD q grammar
  | null last' = Set.singleton (auxiliary [])
  | otherwise = auxiliary `Set.map` last'
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

isTerminalPrefix ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Grammar nt t ->
  t ->
  [Symbol nt t] ->
  Bool
isTerminalPrefix grammar t = bfs Set.empty . Seq.singleton
  where
    derivations' = derivations grammar
    bfs _ Empty = False
    bfs visited (top :<| queue)
      | null top = bfs visited queue
      | top `Set.member` visited = bfs visited queue
      | [head_top] `Set.member` visited = bfs visited queue
      | Terminal t == head_top = True
      | isTerminal $ head top = bfs new_visited queue
      | otherwise = bfs new_visited (queue >< derivations' top)
      where
        head_top = head top
        new_visited =
          if isNonterminal $ head top
            then Set.insert [head_top] new_visited'
            else new_visited'
        new_visited' = Set.insert top visited

solveShortestsPrefix ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Grammar nt t ->
  t ->
  [Symbol nt t] ->
  [Symbol nt t]
solveShortestsPrefix grammar t = solveShortestsPrefix'
  where
    prefixes = reverse . takeWhile (not . List.null) . iterate init
    nullable' = nullable grammar
    isNotTerminalPrefix = not . isTerminalPrefix grammar t
    solveShortestsPrefix' = safeHead . dropWhile isNotTerminalPrefix . prefixes
    safeHead [] = []
    safeHead (x : xs) = x

newLlpItems ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Int ->
  Int ->
  Grammar nt t ->
  [t] ->
  [Symbol nt t] ->
  DotProduction nt t ->
  Set (Item nt t)
newLlpItems q k grammar vi delta dot_production = product'
  where
    product' = Set.fromList [newItem u v | u <- uj, v <- vj]
    last' = last q grammar . (++ alpha) . fmap Terminal
    first' = toList . first k grammar . (x ++) . fmap Terminal
    before' = before q grammar y
    uj = toList . Set.unions $ last' `Set.map` before'
    vj = first' vi
    x = [List.head x_beta | not (List.null x_beta)]
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

addLlpItem ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Int ->
  Grammar nt t ->
  p ->
  Item nt t ->
  Set (Item nt t)
addLlpItem q grammar items old_item
  | null alpha_y = Set.empty
  | isTerminal $ List.last alpha_y = Set.empty
  | otherwise = Set.unions $ newItems <$> deltas
  where
    Item
      { dotProduction = DotProduction x alpha_y beta,
        suffix = u,
        prefix = v,
        shortestPrefix = gamma
      } = old_item
    y = (\(Nonterminal nt) -> nt) $ List.last alpha_y
    productions' = productions grammar
    deltas = symbols <$> findProductions grammar y
    newItems delta = Set.fromList [newItem y | y <- u']
      where
        last' = last q grammar . (++ delta) . fmap Terminal
        before' = before q grammar y
        u' = toList . Set.unions $ last' `Set.map` before'
        newItem z =
          Item
            { dotProduction = DotProduction y delta [],
              suffix = z,
              prefix = v,
              shortestPrefix = gamma
            }

addLlpItems ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Int ->
  Grammar nt t ->
  Set (Item nt t) ->
  Set (Item nt t)
addLlpItems q grammar items = Set.union items $ Set.unions added_items
  where
    added_items = addLlpItem q grammar items `Set.map` items

moveDot :: DotProduction nt t -> Maybe (DotProduction nt t)
moveDot (DotProduction _ [] _) = Nothing
moveDot (DotProduction nt s s') = Just $ DotProduction nt (init s) moved
  where
    moved = List.last s : s'

nextLlpItems ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Int ->
  Int ->
  Grammar nt t ->
  Item nt t ->
  Maybe (Set (Item nt t))
nextLlpItems q k grammar llp_item = newLlpItem' <$> moveDot dot_production
  where
    newLlpItem' = newLlpItems q k grammar v delta
    dot_production = dotProduction llp_item
    v = prefix llp_item
    delta = shortestPrefix llp_item

solveLlpItems ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Int ->
  Int ->
  Grammar nt t ->
  Set (Item nt t) ->
  Set (Set (Item nt t))
solveLlpItems q k grammar =
  Set.map (fixedPointIterate (/=) (addLlpItems q grammar))
    . Set.fromList
    . fmap (Set.unions . mapMaybe (nextLlpItems q k grammar))
    . List.groupBy ((==) `on` auxiliary)
    . List.sortOn auxiliary
    . toList
  where
    auxiliary = safeLast . (\(DotProduction _ xs _) -> xs) . dotProduction
    safeLast [] = Nothing
    safeLast x = Just $ List.last x

llpCollection ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Int ->
  Int ->
  Grammar (AugmentedNonterminal nt) (AugmentedTerminal t) ->
  Set (Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)))
llpCollection q k grammar = removeNull $ auxiliary Set.empty d_init d_init_list
  where
    d_init_list = toList d_init
    removeNull = Set.filter (not . null)
    d_init = Set.singleton $ initD q grammar
    solveLlpItems' = solveLlpItems q k grammar
    auxiliary _ items [] = items
    auxiliary visited items (x : xs)
      | x `Set.member` visited = auxiliary visited items xs
      | otherwise = auxiliary new_visited new_items new_queue
      where
        new_visited = Set.insert x visited
        new_item = solveLlpItems' x
        new_items = Set.union new_item items
        new_queue = xs ++ toList new_item

psls ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Set (Set (Item nt t)) ->
  Map ([t], [t]) (Set [Symbol nt t])
psls = Map.unionsWith Set.union . fmap auxiliary . toList . Set.unions
  where
    auxiliary old_item
      | null alpha_z = Map.empty
      | isTerminal z = Map.singleton (x, y) (Set.singleton gamma)
      | otherwise = Map.empty
      where
        Item
          { dotProduction = DotProduction _ alpha_z _,
            suffix = x,
            prefix = y,
            shortestPrefix = gamma
          } = old_item
        z = List.last alpha_z

llTableParse ::
  (Ord nt, Ord t, Show nt, Show t) =>
  Int ->
  Grammar nt t ->
  [t] ->
  [Symbol nt t] ->
  Maybe ([t], [Symbol nt t], [Int])
llTableParse k grammar a b = auxiliary (a, b, [])
  where
    table = llTable k grammar
    production_map = Map.fromList . zip [0 ..] $ productions grammar
    auxiliary ([], stack, parsed) = Just ([], stack, reverse parsed)
    auxiliary (input, [], parsed) = Just (input, [], reverse parsed)
    auxiliary (x : xs, (Terminal y) : ys, parsed)
      | x == y = Just ([], ys, reverse parsed)
      | otherwise = Nothing
    auxiliary (input, (Nonterminal y) : ys, parsed)
      | isNothing maybeTuple = Nothing
      | otherwise = auxiliary (input, production ++ ys, index : parsed)
      where
        maybeTuple = do
          index <- (y, take k input) `Map.lookup` table
          production <- index `Map.lookup` production_map
          return (index, symbols production)

        Just (index, production) = maybeTuple

allStarts ::
  (Ord nt, Ord t, Show nt, Show t) =>
  Int ->
  Int ->
  Grammar (AugmentedNonterminal nt) (AugmentedTerminal t) ->
  Map ([AugmentedTerminal t], [AugmentedTerminal t]) [Int]
allStarts q k grammar = zero_keys `Map.union` epsilon_keys
  where
    first' = naiveFirst k grammar
    start' = start grammar
    [Production nt s] = findProductions grammar start'
    start_terminals = takeWhile isTerminal s
    isHeadTerminal [a] = isTerminal a
    isHeadTerminal _ = False
    safeInit [] = []
    safeInit a = init a
    epsilon_symbols = safeInit . takeWhile (isHeadTerminal . fst) $ alphaBeta s
    epsilon_keys = Map.fromList $ concatMap auxiliary epsilon_symbols
    zero_symbols = toList $ first' s
    zero_keys = Map.fromList $ (,[0]) . ([],) <$> zero_symbols
    auxiliary (prev, next) = (,[]) . (prev',) <$> toList (first' next)
      where
        prev' = reverse . take q . reverse $ (\(Terminal t) -> t) <$> prev

llpParsingTable ::
  (Ord nt, Ord t, Show nt, Show t) =>
  Int ->
  Int ->
  Grammar (AugmentedNonterminal nt) (AugmentedTerminal t) ->
  Maybe (Map ([AugmentedTerminal t], [AugmentedTerminal t]) [Int])
llpParsingTable q k grammar
  | any ((/= 1) . Set.size) psls_table = Nothing
  | otherwise = Map.union starts <$> sequence parsed
  where
    parsed = Map.mapWithKey auxiliary unwrapped
    unwrapped = (\[a] -> a) . Set.toList <$> psls_table
    starts = allStarts q k grammar
    collection = llpCollection q k grammar
    psls_table = psls collection
    llTableParse' = llTableParse k grammar
    auxiliary (x, y) alpha = f <$> llTableParse' y alpha
      where
        f (epsilon, omega, pi) = pi

pairs :: Int -> Int -> [a] -> [([a], [a])]
pairs q k = toList . auxiliary Seq.empty Seq.empty . Seq.fromList
  where
    auxiliary es _ Empty = es
    auxiliary es ys (x :<| xs) = auxiliary (es :|> substring) (ys :|> x) xs
      where
        backwards = takeR q ys
        forwards = Seq.take (k - 1) xs
        substring = (toList backwards, toList $ x <| forwards)
        takeR i seq = Seq.drop (n - i) seq
          where
            n = Seq.length seq

llpParse :: (Ord nt, Ord t, Show nt, Show t) => Int -> Int -> Grammar nt t -> [t] -> [Int]
llpParse q k grammar = concatMap auxiliary . pairs q k . addStoppers . aug
  where
    addStoppers = (replicate 1 RightTurnstile ++) . (++ replicate 1 LeftTurnstile)
    aug = fmap AugmentedTerminal
    augmented_grammar = augmentGrammar q k grammar
    Just table = llpParsingTable q k augmented_grammar
    auxiliary = (table Map.!)
      -- where
      --   pairs = [(a, b) | a <- tails back, b <- inits forw]
      --   p = List.head $ mapMaybe (`Map.lookup` table) pairs
      --   inits = (++[[]]) . takeWhile (not . null) . iterate init
      --   tails = (++[[]]) . takeWhile (not . null) . iterate tail

leftRecursiveNonterminals :: (Ord nt, Ord t, Show nt, Show t) => Grammar nt t -> [nt]
leftRecursiveNonterminals grammar = mapMaybe (auxiliary . Nonterminal) nonterminals'
  where
    nonterminals' = nonterminals grammar
    leftDerivations' = leftDerivations grammar
    auxiliary nt = bfs Set.empty init_queue
      where
        init_queue = leftDerivations' [nt]
        bfs _ Empty = Nothing
        bfs visited (top :<| queue)
          | null top = bfs visited queue
          | top `Set.member` visited = bfs new_visited queue
          | [head_top] `Set.member` visited = bfs visited queue
          | isTerminal head_top = bfs new_visited queue
          | nt == head_top = Just . (\(Nonterminal a) -> a) $ nt
          | otherwise = bfs new_visited (queue >< leftDerivations' top)
          where
            head_top = head top
            new_visited =
              if isNonterminal $ head top
                then Set.insert [head_top] new_visited'
                else new_visited'
            new_visited' = Set.insert top visited