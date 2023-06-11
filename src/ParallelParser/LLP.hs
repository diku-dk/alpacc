module ParallelParser.LLP
  ( Item (..),
    llpCollection,
    DotProduction (..),
    toDotProduction,
    psls,
    llpParserTableWithStarts,
    llpParse,
    llTableParse,
    initLlpContext,
    llpCollectionMemo,
    llpParserTableWithStartsHomomorphisms,
    Bracket (..),
    LlpContext (..),
    rightNullableDoubleNT,
  )
where

import Control.DeepSeq
import Control.Monad (liftM2)
import Control.Monad.Extra (findM)
import Control.Monad.State
import Control.Parallel.Strategies
import qualified Data.Bifunctor as Bifunctor
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
import Data.String.Interpolate (i)
import Data.Tuple.Extra (both, thd3)
import Debug.Trace (traceShow)
import GHC.Generics
import ParallelParser.Grammar
import ParallelParser.LL
import Prelude hiding (last)

pmap :: NFData b => (a -> b) -> [a] -> [b]
pmap f ls =
  let bs = map f ls
      cs = bs `using` parList rdeepseq
   in cs

debug x = traceShow x x

debugBreak = traceShow "BREAK:"

debugPrint a = traceShow (show a)

-- | This Algebraic data structure is used to model the production with a dot in
-- it descriped in algorithm 8 in the LLP paper.
data DotProduction nt t
  = DotProduction nt [Symbol nt t] [Symbol nt t]
  deriving (Ord, Eq, Read, Generic)

instance (NFData t, NFData nt) => NFData (DotProduction nt t)

-- | Makes the printing of dot productions a bit prettier.
instance (Show nt, Show t) => Show (DotProduction nt t) where
  show (DotProduction nt s s') = [i|#{nt} -> #{s_str}.#{s_str'}|]
    where
      s_str = unwords (map show s)
      s_str' = unwords (map show s')

-- | Converts Productions to DotProductions where the dot is placed on the left
-- side.
toDotProduction :: Production nt t -> DotProduction nt t
toDotProduction (Production nt s) = DotProduction nt s []

-- | This Record models the 4-tuple described in algorithm 8.
data Item nt t = Item
  { dotProduction :: DotProduction nt t,
    suffix :: [t],
    prefix :: [t],
    shortestPrefix :: [Symbol nt t]
  }
  deriving (Eq, Ord, Generic)

instance (NFData t, NFData nt) => NFData (Item nt t)

-- | Prints the Record in the form of the 4-tuple in algorithm 8 in the LLP
-- paper.
instance (Show nt, Show t) => Show (Item nt t) where
  show (Item dp s p sp) = [i|(#{dp}, #{s'}, #{p'}, #{sp'})|]
    where
      s' = unwords (map show s)
      p' = unwords (map show p)
      sp' = unwords (map show sp)
      pprintSet = (++ "}") . ("{" ++) . List.intercalate ", " . map show . Set.toList

-- | Computes the initial item set which is $D_0$ in the LLP paper.
initD :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Set (Item nt t)
initD q grammar = init `Set.map` last'
  where
    production' = head $ findProductions grammar (start grammar)
    last' = last q grammar $ symbols production'
    init l =
      Item
        { dotProduction = toDotProduction production',
          suffix = l,
          prefix = [],
          shortestPrefix = []
        }

-- | Checks if a string of terminals is a prefix of a string symbols. This is
-- done by doing breadth first search on all possible derivations.
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
      | ts' == k_terms_top = True
      | all isTerminal k_terms_top = bfs new_visited queue
      | otherwise = bfs new_visited (queue >< derivations' top)
      where
        k_terms_top = take (length ts) top
        new_visited = Set.insert k_terms_top visited

-- | Given a string of symbols and a string of terminals it solves for the
-- shortest prefix of the symbols where the string of terminals is the prefix.
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

solveShortestsPrefixTest ::
  (Show nt, Show t, Ord nt, Ord t) =>
  [AugmentedTerminal t] ->
  [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)] ->
  State (LlpContext nt t) (Maybe [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)])
solveShortestsPrefixTest vj = findM hasPrefix . prefixes
  where
    hasPrefix w = do
      ctx <- get
      let k = lookahead ctx
      let a = take k vj
      first_set <- useFirst (toList w)
      return $ a `Set.member` first_set
    prefixes = reverse . takeWhile (not . List.null) . iterate init

-- | The context needed for the LLP collection can be created.
data LlpContext nt t = LlpContext
  { -- | The lookback used i.e. q in LLP(q,k)
    lookback :: Int,
    -- | The lookahead used i.e. k in LLP(q,k)
    lookahead :: Int,
    -- | The augmented grammar.
    theGrammar :: Grammar (AugmentedNonterminal nt) (AugmentedTerminal t),
    -- | The context needed for first to be memoized.
    firstContext :: AlphaBetaMemoizedContext (AugmentedNonterminal nt) (AugmentedTerminal t),
    -- | The context needed for first k=1 to be memoized.
    first1Context :: AlphaBetaMemoizedContext (AugmentedNonterminal nt) (AugmentedTerminal t),
    -- | The context needed for last to be memoized.
    lastContext :: AlphaBetaMemoizedContext (AugmentedNonterminal nt) (AugmentedTerminal t),
    -- | The follow function for the given grammar.
    follow' :: AugmentedNonterminal nt -> Set [AugmentedTerminal t],
    -- | The before function for the given grammar.
    before' :: AugmentedNonterminal nt -> Set [AugmentedTerminal t]
  }
  deriving (Generic)

instance (NFData t, NFData nt) => NFData (LlpContext nt t)

-- | Computes the first set within the LLP Context such that memoization can be
-- used.
useFirst ::
  (Ord nt, Ord t) =>
  [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)] ->
  State (LlpContext nt t) (Set [AugmentedTerminal t])
useFirst symbols = do
  ctx <- get
  let first_ctx = firstContext ctx
  let (set, first_ctx') = firstMemoized first_ctx symbols
  let ctx' = ctx {firstContext = first_ctx'}
  put ctx'
  return set

-- | Computes the first k=1 set within the LLP Context such that memoization can
-- be used.
useFirst1 ::
  (Ord nt, Ord t) =>
  [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)] ->
  State (LlpContext nt t) (Set [AugmentedTerminal t])
useFirst1 symbols = do
  ctx <- get
  let first_ctx = first1Context ctx
  let (set, first_ctx') = firstMemoized first_ctx symbols
  let ctx' = ctx {first1Context = first_ctx'}
  put ctx'
  return set

-- | Computes the last set within the LLP Context such that memoization can be
-- used.
useLast ::
  (Ord nt, Ord t) =>
  [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)] ->
  State (LlpContext nt t) (Set [AugmentedTerminal t])
useLast symbols = do
  ctx <- get
  let last_ctx = lastContext ctx
  let (set, last_ctx') = lastMemoized last_ctx symbols
  let ctx' = ctx {lastContext = last_ctx'}
  put ctx'
  return set

-- | Uses the follow function within the LLP context.
useFollow ::
  (Ord nt, Ord t) =>
  AugmentedNonterminal nt ->
  State (LlpContext nt t) (Set [AugmentedTerminal t])
useFollow nt = do
  ctx <- get
  return $ follow' ctx nt

-- | Uses the follow function within the LLP context.
useBefore ::
  (Ord nt, Ord t) =>
  AugmentedNonterminal nt ->
  State (LlpContext nt t) (Set [AugmentedTerminal t])
useBefore nt = do
  ctx <- get
  return $ before' ctx nt

-- | Creates the initial LLP context to be used the LLP collection creation.
initLlpContext ::
  (Ord nt, Ord t, Show nt, Show t) =>
  Int ->
  Int ->
  Grammar nt t ->
  LlpContext nt t
initLlpContext q k grammar =
  LlpContext
    { lookback = q,
      lookahead = k,
      theGrammar = augmented_grammar,
      firstContext = first_ctx,
      first1Context = initFirstMemoizedContext 1 augmented_grammar,
      lastContext = last_ctx,
      before' = beforeFunction,
      follow' = followFunction
    }
  where
    augmented_grammar = augmentGrammar q k grammar
    (first_ctx, followFunction) = firstAndFollow k augmented_grammar
    (last_ctx, beforeFunction) = lastAndBefore q augmented_grammar

-- | From a single LLP item it creates a subset of the set in step 3 (a) of
-- algorithm 8 in the LLP paper. This is done with memoization using the LLP
-- context.
newLlpItemsMemo ::
  (Ord nt, Show nt, Show t, Ord t, NFData t, NFData nt) =>
  Item (AugmentedNonterminal nt) (AugmentedTerminal t) ->
  State (LlpContext nt t) (Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)))
newLlpItemsMemo old_item = result
  where
    Item
      { dotProduction = DotProduction y alpha_x beta,
        suffix = ui,
        prefix = vi,
        shortestPrefix = delta
      } = old_item

    result = do
      ctx <- get
      before_result <- useBefore y
      let before_symbols = toList $ fmap Terminal `Set.map` before_result
      let alpha = if null alpha_x then [] else List.init alpha_x
      uj <- Set.unions <$> mapM (useLast . (++ alpha)) before_symbols
      let x = [List.last alpha_x | not (List.null alpha_x)]
      vj <- useFirst . (x ++) $ fmap Terminal vi
      let x_delta = x ++ delta
      let grammar = theGrammar ctx
      let x_beta = x ++ beta
      let new_dot_production = DotProduction y alpha x_beta
      let newItem' u v = newItem u v new_dot_production x_delta
      let products' = [(u, v) | u <- toList uj, v <- toList vj]
      if null x
        then return Set.empty
        else Set.fromList <$> mapM (uncurry newItem') products'

    newItem u v new_dot_production x_delta = do
      -- ctx <- get
      -- let grammar = theGrammar ctx
      -- let shortest_prefix = solveShortestsPrefix 1 grammar v x_delta
      shortest_prefix <- solveShortestsPrefixTest v x_delta
      return
        Item
          { dotProduction = new_dot_production,
            suffix = u,
            prefix = v,
            shortestPrefix = fromJust shortest_prefix
          }

-- | From a single LLP item it creates a subset of the set in step 3 (a) of
-- algorithm 8 in the LLP paper.
newLlpItems ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Int ->
  Int ->
  Grammar (AugmentedNonterminal nt) (AugmentedTerminal t) ->
  Item (AugmentedNonterminal nt) (AugmentedTerminal t) ->
  Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t))
newLlpItems q k grammar old_item
  | null x = Set.empty
  | otherwise = product'
  where
    product' = Set.fromList [newItem u v | u <- uj, v <- vj]
    last' = last q grammar . (++ alpha)
    first' = first k grammar . (x ++)
    before' = fmap Terminal `Set.map` before q grammar y
    uj = toList . Set.unions $ last' `Set.map` before'
    vj = toList . first' $ fmap Terminal vi
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

-- | This function performs step 3 (b) in algorithm 8 of the LLP paper. This is
-- done for a single item and if the item does not fulfill the conditions
-- described in the paper, the empty set is returned. This is done with
-- memozation.
extraLlpItemMemo ::
  (Ord nt, Show nt, Show t, Ord t, NFData t, NFData nt) =>
  Item (AugmentedNonterminal nt) (AugmentedTerminal t) ->
  State (LlpContext nt t) (Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)))
extraLlpItemMemo old_item
  | null alpha_y = return Set.empty
  | isTerminal $ List.last alpha_y = return Set.empty
  | otherwise = result
  where
    Item
      { dotProduction = DotProduction x alpha_y beta,
        suffix = u,
        prefix = v,
        shortestPrefix = gamma
      } = old_item

    newItem delta u =
      Item
        { dotProduction = DotProduction y delta [],
          suffix = u,
          prefix = v,
          shortestPrefix = gamma
        }

    Nonterminal y = List.last alpha_y

    newItems delta = result
      where
        result = do
          before_result <- useBefore y
          let before_symbols = toList $ fmap Terminal `Set.map` before_result
          u' <- mapM (useLast . (++ delta)) before_symbols
          return $ newItem delta `Set.map` Set.unions u'

    result = do
      ctx <- get
      let grammar = theGrammar ctx
      let deltas = symbols <$> findProductions grammar y
      Set.unions <$> mapM newItems deltas

-- | This function performs step 3 (b) in algorithm 8 of the LLP paper. This is
-- done for a single item and if the item does not fulfill the conditions
-- described in the paper, the empty set is returned.
extraLlpItem ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Int ->
  Grammar (AugmentedNonterminal nt) (AugmentedTerminal t) ->
  Item (AugmentedNonterminal nt) (AugmentedTerminal t) ->
  Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t))
extraLlpItem q grammar old_item
  | null alpha_y = Set.empty
  | isTerminal $ List.last alpha_y = Set.empty
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

-- | This function performs step 3 (b) of algorithm 8 in the LLP paper. This is
-- with memoization.
extraLlpItemsMemo ::
  (Ord nt, Show nt, Show t, Ord t, NFData t, NFData nt) =>
  Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)) ->
  State (LlpContext nt t) (Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)))
extraLlpItemsMemo = fixedPointIterate
  where
    fixedPointIterate items = do
      new_items <- addExtraItems items
      if items == new_items
        then return new_items
        else fixedPointIterate new_items

    addExtraItems items = do
      extras <- mapM extraLlpItemMemo $ toList items
      return $ items `Set.union` Set.unions extras

-- | This function performs step 3 (b) of algorithm 8 in the LLP paper.
extraLlpItems ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Int ->
  Grammar (AugmentedNonterminal nt) (AugmentedTerminal t) ->
  Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)) ->
  Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t))
extraLlpItems q grammar = fixedPointIterate (/=) addedItems
  where
    addedItems items = items `Set.union` Set.unions (extraLlpItem q grammar `Set.map` items)

-- | This function computes a single iteration of step 3 for a single item in
-- algorithm 8 of the LLP paper. This will create a subset of a item set. This
-- is done using memoization.
solveLlpItemMemo ::
  (Ord nt, Show nt, Show t, Ord t, NFData t, NFData nt) =>
  Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)) ->
  State (LlpContext nt t) (Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)))
solveLlpItemMemo items = do
  ctx <- get
  new_items <- mapM newLlpItemsMemo $ toList items
  extraLlpItemsMemo $ Set.unions new_items

-- | This function computes a single iteration of step 3 for a single item in
-- algorithm 8 of the LLP paper. This will be a subset of a item set.
solveLlpItem ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Int ->
  Int ->
  Grammar (AugmentedNonterminal nt) (AugmentedTerminal t) ->
  Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)) ->
  Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t))
solveLlpItem q k grammar items =
  extraLlpItems q grammar
    . Set.unions
    $ Set.map (newLlpItems q k grammar) items

-- | Given a item set it creates a new set of items sets which will be a subset
-- of the LLP collection. This corrosponds to a single iteration of step 3. This
-- is done using memoization.
solveLlpItemsMemo ::
  (Ord t, Ord nt, Show nt, Show t, NFData t, NFData nt) =>
  Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)) ->
  State (LlpContext nt t) (Set (Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t))))
solveLlpItemsMemo items = do
  let groups = groupByDotSymbol Map.empty $ toList items
  Set.fromList <$> mapM solveLlpItemMemo (toList groups)
  where
    auxiliary =
      safeLast
        . (\(DotProduction _ alpha_x _) -> alpha_x)
        . dotProduction
    safeLast [] = []
    safeLast x = [List.last x]
    groupByDotSymbol symbol_map [] = symbol_map
    groupByDotSymbol symbol_map (a : as)
      | null x' = groupByDotSymbol symbol_map as
      | x `Map.member` symbol_map = groupByDotSymbol new_symbol_map as
      | otherwise = groupByDotSymbol new_symbol_map' as
      where
        new_symbol_map = Map.adjust (Set.insert a) x symbol_map
        new_symbol_map' = Map.insert x (Set.singleton a) symbol_map
        [x] = x'
        x' = auxiliary a

-- | Given a item set it creates a new set of items sets which will be a subset
-- of the LLP collection. This corrosponds to a single iteration of step 3.
solveLlpItems ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Int ->
  Int ->
  Grammar (AugmentedNonterminal nt) (AugmentedTerminal t) ->
  Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)) ->
  Set (Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)))
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
    groupByDotSymbol symbol_map (a : as)
      | null x' = groupByDotSymbol symbol_map as
      | x `Map.member` symbol_map = groupByDotSymbol new_symbol_map as
      | otherwise = groupByDotSymbol new_symbol_map' as
      where
        new_symbol_map = Map.adjust (Set.insert a) x symbol_map
        new_symbol_map' = Map.insert x (Set.singleton a) symbol_map
        [x] = x'
        x' = auxiliary a

solveLlpItemsMemoRunState ::
  (Ord t, Ord nt, Show nt, Show t, NFData t, NFData nt) =>
  LlpContext nt t ->
  Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)) ->
  (Set (Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t))), LlpContext nt t)
solveLlpItemsMemoRunState ctx set = runState (solveLlpItemsMemo set) ctx

solveLlpItemMemoParallel ::
  (Ord nt, Show nt, Ord t, Show t, NFData t, NFData nt) =>
  [Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t))] ->
  State (LlpContext nt t) (Set (Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t))))
solveLlpItemMemoParallel sets = do
  ctx <- get
  let (sets', ctxs') = unzip $ parMap rdeepseq (solveLlpItemsMemoRunState ctx) sets
  let first_state = Map.unions $ alphaBetaState . firstContext <$> ctxs'
  let last_state = Map.unions $ alphaBetaState . lastContext <$> ctxs'
  let new_ctx = ctx {firstContext = (firstContext ctx) {alphaBetaState = first_state}}
  let new_ctx' = ctx {lastContext = (lastContext ctx) {alphaBetaState = last_state}}
  put new_ctx'
  return $ Set.unions sets'

-- | Creates the LLP collection as described in algorithm 8 from the LLP paper.
-- This is done using memoization.
llpCollectionMemoParallel ::
  (Ord t, Ord nt, Show nt, Show t, NFData t, NFData nt) =>
  State (LlpContext nt t) (Set (Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t))))
llpCollectionMemoParallel = do
  ctx <- get
  let grammar = theGrammar ctx
  let q = lookback ctx
  let d_init = initD q grammar
  let d_init_set = Set.singleton d_init
  let d_init_list = [d_init]
  removeNull <$> auxiliary Set.empty d_init_set d_init_list
  where
    removeNull = Set.filter (not . null)
    auxiliary _ items [] = return items
    auxiliary visited items queue = do
      let not_visited = filter (`Set.notMember` visited) queue
      new_items' <- solveLlpItemMemoParallel not_visited
      let new_queue = toList new_items'
      let new_items = items `Set.union` new_items'
      let new_visited = Set.union (Set.fromList queue) visited
      auxiliary new_visited new_items new_queue

-- | Creates the LLP collection as described in algorithm 8 from the LLP paper.
-- This is done using memoization.
llpCollectionMemo ::
  (Ord t, Ord nt, Show nt, Show t, NFData t, NFData nt) =>
  State (LlpContext nt t) (Set (Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t))))
llpCollectionMemo = do
  ctx <- get
  let grammar = theGrammar ctx
  let q = lookback ctx
  let d_init = initD q grammar
  let d_init_set = Set.singleton d_init
  let d_init_list = [d_init]
  removeNull <$> auxiliary Set.empty d_init_set d_init_list
  where
    removeNull = Set.filter (not . null)
    auxiliary _ items [] = return items
    auxiliary visited items (top : queue) = do
      if top `Set.member` visited
        then auxiliary visited items queue
        else new
      where
        new_visited = Set.insert top visited
        new = do
          new_item <- solveLlpItemsMemo top
          let new_items = Set.union new_item items
          let new_queue = queue ++ toList new_item
          auxiliary new_visited new_items new_queue

-- | Creates the LLP collection as described in algorithm 8 from the LLP paper.
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

-- | Creates the PSLS table as described in algorithm 9 in the LLP paper.
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

unionIfDisjoint :: Ord a => Set a -> Set a -> Maybe (Set a)
unionIfDisjoint a b = 
  if a `Set.disjoint` b
    then Just $ a `Set.union` b
    else Nothing 

unionsIfDisjoint :: Ord a =>  [Set a] -> Maybe (Set a)
unionsIfDisjoint = foldM unionIfDisjoint Set.empty

-- | Creates a LL(k) table for a given grammar. It does not check for conflicts.
llTableLlpContext ::
  (Ord nt, Ord t, Show nt, Show t) =>
  State (LlpContext nt t) (Maybe (Map (AugmentedNonterminal nt, [AugmentedTerminal t]) Int))
llTableLlpContext = do
  ctx <- get
  let k = lookahead ctx
  let prods = productions (theGrammar ctx)
  tables <- zipWithM (tableEntry k) [0 ..] prods
  return $ do
    let keys = Map.keysSet <$> tables
    let result = unionsIfDisjoint keys
    case result of
      Just _ -> Just $ Map.unions tables
      _ -> Nothing
  where
    tableEntry k i (Production nt a) = do
      first_set <- useFirst a
      follow_set <- useFollow nt
      let first_follow_prod = toList $ truncatedProduct k first_set follow_set
      return $ Map.fromList [((nt, y), i) | y <- first_follow_prod]

-- | Performance the parsing described in step 2 of algorithm 13 of the LLP
-- paper.
llTableParse ::
  (Ord nt, Ord t, Show nt, Show t) =>
  Map (nt, [t]) Int ->
  Int ->
  Grammar nt t ->
  [t] ->
  [Symbol nt t] ->
  ([t], [Symbol nt t], [Int])
llTableParse table k grammar a b = parse (a, b, [])
  where
    production_map = Map.fromList . zip [0 ..] $ productions grammar
    parse (x : xs, (Terminal y) : ys, parsed)
      | x == y = ([], ys, reverse parsed)
      | otherwise = error "Internal error during llTableParse."
    parse (input, (Nonterminal y) : ys, parsed)
      | isNothing maybeTuple = error "Internal error during llTableParse."
      | otherwise = parse (input, production ++ ys, index : parsed)
      where
        Just (index, production) = maybeTuple
        maybeTuple = do
          index <- (y, input) `Map.lookup` table
          production <- index `Map.lookup` production_map
          return (index, symbols production)
    parse ([], [], parsed) = ([], [], reverse parsed)
    parse (_, _, _) = error "Internal error during llTableParse."

-- | Creates all the starting pairs which results in the augmented starting
-- production.
allStarts ::
  (Ord nt, Ord t, Show nt, Show t) =>
  State
    (LlpContext nt t)
    ( Map
        ([AugmentedTerminal t], [AugmentedTerminal t])
        ( [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)],
          [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)],
          [Int]
        )
    )
allStarts = do
  ctx <- get
  let grammar = theGrammar ctx
  let start' = start grammar
  let [Production nt s] = findProductions grammar start'
  zero_symbols <- toList <$> useFirst s
  let zero_keys = Map.fromList $ (,([Nonterminal start'], tail s, [0])) . ([],) <$> zero_symbols
  return zero_keys

-- | Removes the keys of a LLP table if the pairs are not admissable i.e. cannot
-- be created with the grammar.
filterAdmissiblePairs ::
  (Show nt, Show t, Ord nt, Ord t) =>
  Int ->
  Int ->
  Grammar nt t ->
  Map ([t], [t]) a ->
  Map ([t], [t]) a
filterAdmissiblePairs q k grammar = Map.filterWithKey (\k _ -> isValid k)
  where
    valid_strings = validLlSubstrings (q + k) grammar
    isValid (x, y) =
      xy `Set.member` valid_strings || any (xy `List.isInfixOf`) valid_strings
      where
        xy = x ++ y

llpParserTableWithStarts ::
  (Ord nt, Ord t, Show nt, Show t, NFData t, NFData nt) =>
  Int ->
  Int ->
  Grammar nt t ->
  Maybe
    ( Map
        ([AugmentedTerminal t], [AugmentedTerminal t])
        ( [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)],
          [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)],
          [Int]
        )
    )
llpParserTableWithStarts q k grammar = (`Map.union` starts) <$> maybe_table
  where
    init_context = initLlpContext q k grammar
    (maybe_table, starts) = flip evalState init_context $ do
      maybe_table' <- llpParserTable
      starts' <- allStarts
      return (maybe_table', starts')

data Bracket a = LBracket a | RBracket a deriving (Eq, Ord, Show, Functor)

llpParserTableWithStartsHomomorphisms ::
  (NFData t, NFData nt, Ord nt, Show nt, Show t, Ord t) =>
  Int ->
  Int ->
  Grammar nt t ->
  Maybe
    ( Map
        ([AugmentedTerminal t], [AugmentedTerminal t])
        ( [Bracket (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t))],
          [Int]
        )
    )
llpParserTableWithStartsHomomorphisms q k grammar = result
  where
    result = (`Map.union` starts) <$> maybe_table
    init_context = initLlpContext q k grammar
    (maybe_table, starts) = flip evalState init_context $ do
      maybe_table' <- llpParserTable
      starts' <- allStarts
      return (fmap homomorphisms <$> maybe_table', start_homomorphisms <$> starts')
    start_homomorphisms (_, omega, pi) = (LBracket <$> reverse omega, pi)
    homomorphisms (alpha, omega, pi) = (right ++ left, pi)
      where
        left = LBracket <$> reverse omega
        right = RBracket <$> alpha

-- | Creates the LLP parsing table if Nothing is returned then the table could
-- not be created since the grammar is not LLP(q,k).
llpParserTable ::
  (Ord nt, Ord t, Show nt, Show t, NFData t, NFData nt) =>
  State
    (LlpContext nt t)
    ( Maybe
        ( Map
            ([AugmentedTerminal t], [AugmentedTerminal t])
            ([Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)], [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)], [Int])
        )
    )
llpParserTable = do
  context <- get
  let q = lookback context
  let k = lookahead context
  let grammar = theGrammar context
  maybe_table <- llTableLlpContext
  let Just table = maybe_table
  let llTableParse' = llTableParse table k (theGrammar context)
  let auxiliary (x, y) alpha = f $ llTableParse' y alpha
        where
          f (epsilon, omega, pi) = (alpha, omega, pi)
  collection <- llpCollectionMemo -- llpCollectionMemoParallel -- llpCollection q k (theGrammar context)
  let psls_table = psls collection -- filterAdmissiblePairs q k grammar $ 
  let unwrapped = (\[a] -> a) . Set.toList <$> psls_table
  let parsed = Map.mapWithKey auxiliary unwrapped
  let result
        | isNothing maybe_table = Nothing
        | any ((/= 1) . Set.size) psls_table = Nothing
        | otherwise = Just parsed
  return result

-- | Given a lsit create all the pairs with q lookback and k lookahead which
-- will be used as keys in the table.
pairLookup :: (Ord t, Show t) => Map ([t], [t]) v -> Int -> Int -> [t] -> [Maybe v]
pairLookup table q k = toList . auxiliary Seq.empty Seq.empty . Seq.fromList
  where
    auxiliary es _ Empty = es
    auxiliary es ys (x :<| xs) = auxiliary (es :|> val) (ys :|> x) xs
      where
        backwards = takeR q ys
        forwards = Seq.take (k - 1) xs
        val = (toList backwards, toList $ x <| forwards) `Map.lookup` table
        takeR i seq = Seq.drop (n - i) seq
          where
            n = Seq.length seq

-- | glues two stacks together as described in definition 1 of the LLP paper.
glue ::
  (Eq nt, Eq t) =>
  ([Symbol nt t], [Symbol nt t], [Int]) ->
  ([Symbol nt t], [Symbol nt t], [Int]) ->
  Maybe ([Symbol nt t], [Symbol nt t], [Int])
glue (alpha_l, omega_l, pi_l) (alpha_r, omega_r, pi_r)
  | omega_l == alpha_r = Just (alpha_l, omega_r, pi)
  | omega_l `List.isPrefixOf` alpha_r = Just (alpha_l ++ beta_0, omega_r, pi)
  | alpha_r `List.isPrefixOf` omega_l = Just (alpha_l, omega_r ++ beta_1, pi)
  | otherwise = Nothing
  where
    pi = pi_l ++ pi_r
    beta_0 = drop (length omega_l) alpha_r
    beta_1 = drop (length alpha_r) omega_l

-- | Given a grammar it will parse the string using a LLP table.
llpParse :: (Ord nt, Show nt, Show t, Ord t, NFData t, NFData nt) => Int -> Int -> Grammar nt t -> [t] -> Maybe [Int]
llpParse q k grammar string = fmap thd3 . foldl1 glue' . pairLookup table q k . addStoppers $ aug string
  where
    addStoppers = (replicate 1 RightTurnstile ++) . (++ replicate 1 LeftTurnstile)
    aug = fmap AugmentedTerminal
    augmented_grammar = augmentGrammar q k grammar
    Just table = llpParserTableWithStarts q k grammar
    glue' a b = do
      a' <- a
      b' <- b
      glue a' b'

rightNullableDoubleNT :: (Show nt, Show t, Ord t, Ord nt) => Grammar nt t -> Bool
rightNullableDoubleNT grammar = any isProblem prods
  where
    consecNt (Nonterminal a) (Nonterminal b) = a == b
    consecNt _ _ = False

    nts = nonterminals grammar
    prods = productions grammar
    nullable' = nullable grammar
    production_set = Set.fromList $ productions grammar
    problem_nts = Set.map nonterminal $ Set.filter isProblem production_set
    graph = makeGraph $ productions grammar
    empty_graph = Map.unions $ map (`Map.singleton` Set.empty) nts
    addMissing = Map.unionWith Set.union empty_graph

    leftNode (Production nt s) = rightNode (Production nt (reverse s))
    rightNode (Production nt s) =
      Map.singleton nt
        . Set.insert nt
        . Set.fromList
        $ fst <$> right_symbols
      where
        right_symbols = List.filter (nullable' . snd) $ rightSymbols s

    left_graph = addMissing . Map.unionsWith Set.union $ leftNode <$> prods
    right_graph = addMissing . Map.unionsWith Set.union $ rightNode <$> prods
    left_graph_trans = fixedPointIterate (/=) closure left_graph
    right_graph_trans = fixedPointIterate (/=) closure right_graph

    isProblem (Production nt s)
      | null s = False
      | otherwise =
          any predicate
            . catMaybes
            $ List.zipWith createPairs init' tails
      where
        seq = Seq.fromList s
        tails = toList . fmap toList . Seq.drop 1 $ Seq.tails seq
        init' = List.init s
        createPairs (Nonterminal before) ((Nonterminal head) : tail) =
          Just ((before, head), tail)
        createPairs _ _ = Nothing
        predicate ((a, b), tail) =
          nullable' tail
            && shares_a_nt
          where
            right_set = right_graph_trans Map.! a
            left_set = left_graph_trans Map.! b
            shares_a_nt = not . null $ Set.intersection right_set left_set

    makeGraph =
      addMissing
        . Map.unionsWith Set.union
        . fmap makeNode
      where
        nts = nonterminals grammar

    makeNode (Production nt s) = Map.singleton nt end_nts
      where
        seq = Seq.fromList s
        tails = toList . fmap toList $ Seq.tails seq
        end_nts = Set.fromList $ mapMaybe endNt tails
        endNt ((Nonterminal nt) : xs)
          | nullable' xs = Just nt
          | otherwise = Nothing
        endNt _ = Nothing

    closure old_map = addOldMap $ addMissing <$> old_map
      where
        addOldMap = Map.unionWith Set.union old_map
        addMissing = Set.unions . Set.map (old_map Map.!)

    reachable_graph = fixedPointIterate (/=) closure graph
    containsProblem = flip Set.member (reachable_graph Map.! start grammar)