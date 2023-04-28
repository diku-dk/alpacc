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
import Data.String.Interpolate (i)
import Control.Parallel.Strategies

pmap :: NFData b => (a -> b) -> [a] -> [b]
pmap f ls =
        let bs = map f ls
            cs = bs `using` parList rdeepseq
        in cs

debug x = traceShow x x
debugBreak = traceShow "BREAK:"
debugPrint a = traceShow (show a)

data DotProduction nt t
  = DotProduction nt [Symbol nt t] [Symbol nt t]
  deriving (Ord, Eq, Read)

instance (Show nt, Show t) => Show (DotProduction nt t) where
  show (DotProduction nt s s') = [i|#{nt} -> #{unwords (map show s)}.#{unwords (map show s')}|]

toDotProduction :: Production nt t -> DotProduction nt t
toDotProduction (Production nt s) = DotProduction nt s []

data Item nt t = Item
  { dotProduction :: DotProduction nt t,
    suffix :: [t],
    prefix :: [t],
    shortestPrefix :: [Symbol nt t]
  }
  deriving (Eq, Ord)

instance (Show nt, Show t) => Show (Item nt t) where
  show (Item dp s p sp) = [i|(#{dp}, #{unwords (map show s)}, #{unwords (map show p)}, #{unwords (map show sp)})|]
    where
      pprintSet = (++ "}") . ("{" ++) . List.intercalate ", " . map show . Set.toList

initD :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Set (Item nt t)
initD q grammar = init `Set.map` last'
  where
    production' = head $ findProductions grammar (start grammar)
    last' = last q grammar $ symbols production'
    init l = Item
        { dotProduction = toDotProduction production',
          suffix = l,
          prefix = [],
          shortestPrefix = []
        }

isTerminalPrefix ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Grammar nt t ->
  [t] ->
  [Symbol nt t] ->
  Bool
isTerminalPrefix grammar ts = bfs Set.empty . Seq.singleton
  where
    ts' = Terminal <$> ts
    derivations' = derivations grammar
    bfs _ Empty = False
    bfs visited (top :<| queue)
      | null top = bfs visited queue
      | k_terms_top `Set.member` visited = bfs visited queue
      |  ts' == k_terms_top = True
      | all isTerminal k_terms_top = bfs new_visited queue
      | otherwise = bfs new_visited (queue >< derivations' top)
      where
        k_terms_top = take (length ts) top
        new_visited = Set.insert k_terms_top visited

solveShortestsPrefix ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Int ->
  Grammar nt t ->
  [t] ->
  [Symbol nt t] ->
  [Symbol nt t]
solveShortestsPrefix _ _ [] _ = []
solveShortestsPrefix k grammar ts string = solveShortestsPrefix' string
  where
    ts' = take k ts
    prefixes = reverse . takeWhile (not . List.null) . iterate init
    nullable' = nullable grammar
    isNotTerminalPrefix = not . isTerminalPrefix grammar ts'
    solveShortestsPrefix' = safeHead . dropWhile isNotTerminalPrefix . prefixes
    safeHead [] = []
    safeHead (x : xs) = x

newLlpItems ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Int ->
  Int ->
  Grammar nt t ->
  Item nt t ->
  Set (Item nt t)
newLlpItems q k grammar old_item
  | null x = Set.empty
  | otherwise = product'
  where
    product' = Set.fromList [newItem u v | u <- uj, v <- vj]
    last' = last q grammar . (++ alpha)
    first' = first k grammar . (x ++)
    before' = fmap Terminal `Set.map` before q grammar y
    uj = toList . Set.unions $ last' `Set.map` before'
    vj = toList $ first' $ fmap Terminal vi
    x = [List.last alpha_x | not (List.null alpha_x)]
    alpha = if null alpha_x then [] else List.init alpha_x
    x_delta = x ++ delta
    solveShortestsPrefix' v = solveShortestsPrefix 1 grammar v x_delta
    x_beta = x ++ beta
    new_dot_production = DotProduction y alpha x_beta
    newItem u v =
      Item
        { dotProduction = new_dot_production,
          suffix = u,
          prefix = v,
          shortestPrefix = solveShortestsPrefix' v
        }
    Item
      { dotProduction = DotProduction y alpha_x beta,
        suffix = ui,
        prefix = vi,
        shortestPrefix = delta
      } = old_item

extraLlpItem ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Int ->
  Grammar nt t ->
  Item nt t ->
  Set (Item nt t)
extraLlpItem q grammar old_item
  | null alpha_y = Set.empty
  | isTerminal $ List.last alpha_y = Set.empty
  | y == start grammar = Set.empty
  | otherwise = Set.unions $ newItems `Set.map` deltas
  where
    Item
      { dotProduction = DotProduction x alpha_y beta,
        suffix = u,
        prefix = v,
        shortestPrefix = gamma
      } = old_item
    Nonterminal y = List.last alpha_y
    productions' = productions grammar
    deltas = Set.fromList $ symbols <$> findProductions grammar y
    newItems delta = newItem `Set.map` u'
      where
        last' = last q grammar . (++ delta)
        before' = fmap Terminal `Set.map` before q grammar y
        u' = Set.unions $ last' `Set.map` before'
        newItem u =
          Item
            { dotProduction = DotProduction y delta [],
              suffix = u,
              prefix = v,
              shortestPrefix = gamma
            }

extraLlpItems ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Int ->
  Grammar nt t ->
  Set (Item nt t) ->
  Set (Item nt t)
extraLlpItems q grammar = fixedPointIterate (/=) addedItems
  where
    addedItems items = items `Set.union` Set.unions (extraLlpItem q grammar `Set.map` items)

solveLlpItem ::
  (Ord t, Ord nt, Show nt, Show t) => Int ->
  Int ->
  Grammar nt t ->
  Set (Item nt t) ->
  Set (Item nt t)
solveLlpItem q k grammar items =
  extraLlpItems q grammar
    . Set.unions
    $ Set.map (newLlpItems q k grammar) items

solveLlpItems :: (Ord t, Ord nt, Show nt, Show t) =>
  Int ->
  Int ->
  Grammar nt t ->
  Set (Item nt t) ->
  Set (Set (Item nt t))
solveLlpItems q k grammar =
  Set.fromList
    . toList
    . Map.map (solveLlpItem q k grammar)
    . groupByDotSymbol Map.empty
    . toList
  where
    auxiliary =
      safeLast
      . (\(DotProduction _ alpha_x _) -> alpha_x)
      . dotProduction
    safeLast [] = []
    safeLast x = [List.last x]
    groupByDotSymbol symbol_map [] = symbol_map
    groupByDotSymbol symbol_map (a:as)
      | null x' = groupByDotSymbol symbol_map as
      | x `Map.member` symbol_map = groupByDotSymbol new_symbol_map as
      | otherwise = groupByDotSymbol new_symbol_map' as
      where
        new_symbol_map = Map.adjust (Set.insert a) x symbol_map
        new_symbol_map' = Map.insert x (Set.singleton a) symbol_map
        [x] = x'
        x' = auxiliary a

llpCollection ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Int ->
  Int ->
  Grammar (AugmentedNonterminal nt) (AugmentedTerminal t) ->
  Set (Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)))
llpCollection q k grammar = removeNull $ auxiliary Set.empty d_init_set d_init_list
  where
    d_init_list = [d_init]
    removeNull = Set.filter (not . null)
    d_init_set = Set.singleton d_init
    d_init = initD q grammar
    solveLlpItems' = solveLlpItems q k grammar
    auxiliary _ items [] = items
    auxiliary visited items (top : queue)
      | top `Set.member` visited = auxiliary visited items queue
      | otherwise = auxiliary new_visited new_items new_queue
      where
        new_visited = Set.insert top visited
        new_item = solveLlpItems' top
        new_items = Set.union new_item items
        new_queue = queue ++ toList new_item

psls ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Set (Set (Item nt t)) ->
  Map ([t], [t]) (Set [Symbol nt t])
psls = Map.unionsWith Set.union . fmap auxiliary . toList . Set.unions
  where
    auxiliary old_item
      | null alpha_z = Map.empty
      | null y = Map.empty
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
        keys =
          filter (`Map.member` table)
            . fmap (y,)
            . takeWhile (not . null)
            . iterate init
            $ take k input

        safeHead [] = Nothing
        safeHead (x:_) = Just x

        maybeTuple = do
          key <- safeHead keys
          index <- key `Map.lookup` table
          production <- index `Map.lookup` production_map
          return (index, symbols production)

        Just (index, production) = maybeTuple

allStarts ::
  (Ord nt, Ord t, Show nt, Show t) =>
  Int ->
  Int ->
  Grammar (AugmentedNonterminal nt) (AugmentedTerminal t) ->
  Map ([AugmentedTerminal t], [AugmentedTerminal t]) [Int]
allStarts q k grammar = zero_keys
  where
    first' = naiveFirst k grammar
    start' = start grammar
    [Production nt s] = findProductions grammar start'
    zero_symbols = toList $ first' s
    zero_keys = Map.fromList $ (,[0]) . ([],) <$> zero_symbols

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
llpParse q k grammar [] = []
llpParse q k grammar string = concatMap auxiliary . pairs q k . addStoppers $ aug string
  where
    addStoppers = ([RightTurnstile] ++) . (++ [LeftTurnstile])
    aug = fmap AugmentedTerminal
    augmented_grammar = augmentGrammar grammar
    Just table = llpParsingTable q k augmented_grammar
    auxiliary = (table Map.!) . debug
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
          | head_top `Set.member` visited = bfs visited queue
          | isTerminal head_top = bfs new_visited queue
          | nt == head_top = Just . unpackNonterminal $ nt
          | otherwise = bfs new_visited (queue >< leftDerivations' top)
          where
            head_top = head top
            new_visited = Set.insert head_top visited