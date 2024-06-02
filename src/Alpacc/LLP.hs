module Alpacc.LLP
  ( Item (..),
    DotProduction (..),
    toDotProduction,
    psls,
    llpParserTableWithStarts,
    llpParse,
    initLlpContext,
    llpCollectionMemo,
    llpParserTableWithStartsHomomorphisms,
    Bracket (..),
    LlpContext (..),
  )
where

import Control.DeepSeq
import Control.Monad.State
import Data.Bifunctor qualified as BI
import Data.Either.Extra (maybeToEither)
import Data.Foldable
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Maybe
import Data.Sequence (Seq (..), (<|))
import Data.Sequence qualified as Seq hiding (Seq (..), (<|), (><), (|>))
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.String.Interpolate (i)
import Data.Tuple.Extra (fst3, thd3, both)
import GHC.Generics
import Alpacc.Grammar
import Alpacc.LL hiding (before, follow, llParse)
import Prelude hiding (last)

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
    llpConfig :: ([Symbol nt t], [Symbol nt t], [Int])
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
      sp' = unwords (map show $ fst3 sp)

-- | Computes the initial item set which is $D_0$ in the LLP paper.
initD :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Set (Item nt t)
initD q grammar = init' `Set.map` last'
  where
    production' = head $ findProductions grammar (start grammar)
    last' = last q grammar $ symbols production'
    init' l =
      Item
        { dotProduction = toDotProduction production',
          suffix = l,
          prefix = [],
          llpConfig = ([], [], [])
        }

solveShortestsPrefix ::
  (Show nt, Show t, Ord nt, Ord t) =>
  [AugmentedTerminal t] ->
  [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)] ->
  State
    (LlpContext nt t)
    ( Maybe
        ( [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)],
          [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)],
          [Int]
        )
    )
solveShortestsPrefix vj =
  fmap (join . find isJust)
    . mapM (useParseLlpConfig vj)
    . prefixes
  where
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
    -- | The context needed for last to be memoized.
    lastContext :: AlphaBetaMemoizedContext (AugmentedNonterminal nt) (AugmentedTerminal t),
    -- | The follow function for the given grammar.
    follow :: AugmentedNonterminal nt -> Set [AugmentedTerminal t],
    -- | The before function for the given grammar.
    before :: AugmentedNonterminal nt -> Set [AugmentedTerminal t],
    -- | The LL(k) table for the grammar.
    table :: Map (AugmentedNonterminal nt, [AugmentedTerminal t]) Int
  }
  deriving (Generic)

instance (NFData t, NFData nt) => NFData (LlpContext nt t)

instance (Ord t, Ord nt) => Semigroup (LlpContext nt t) where
  a <> b = a {firstContext = new_first_ctx, lastContext = new_last_ctx}
    where
      a_first_ctx = firstContext a
      b_first_ctx = firstContext b
      new_first_ctx = a_first_ctx <> b_first_ctx
      a_last_ctx = lastContext a
      b_last_ctx = lastContext b
      new_last_ctx = a_last_ctx <> b_last_ctx

-- | Computes the first set within the LLP Context such that memoization can be
-- used.
useFirst ::
  (Ord nt, Ord t) =>
  [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)] ->
  State (LlpContext nt t) (Set [AugmentedTerminal t])
useFirst _symbols = do
  ctx <- get
  let first_ctx = firstContext ctx
  let (set, first_ctx') = firstMemoized first_ctx _symbols
  let ctx' = ctx {firstContext = first_ctx'}
  put ctx'
  return set

-- | Computes the last set within the LLP Context such that memoization can be
-- used.
useLast ::
  (Ord nt, Ord t) =>
  [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)] ->
  State (LlpContext nt t) (Set [AugmentedTerminal t])
useLast _symbols = do
  ctx <- get
  let last_ctx = lastContext ctx
  let (set, last_ctx') = lastMemoized last_ctx _symbols
  let ctx' = ctx {lastContext = last_ctx'}
  put ctx'
  return set

-- | Uses the follow function within the LLP context.
-- useFollow ::
--   (Ord nt, Ord t) =>
--   AugmentedNonterminal nt ->
--   State (LlpContext nt t) (Set [AugmentedTerminal t])
-- useFollow nt = do
--   ctx <- get
--   return $ follow ctx nt

-- | Uses the LL(k) parse function within the LLP context.
useParseLlpConfig ::
  (Ord nt, Ord t, Show nt, Show t) =>
  [AugmentedTerminal t] ->
  [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)] ->
  State (LlpContext nt t) (Maybe ([Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)], [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)], [Int]))
useParseLlpConfig a b = do
  ctx <- get
  let k = lookahead ctx
  let grammar = theGrammar ctx
  let table' = table ctx
  return $ parseLlpConfig table' k grammar a b

-- | Uses the follow function within the LLP context.
useBefore ::
  (Ord nt, Ord t) =>
  AugmentedNonterminal nt ->
  State (LlpContext nt t) (Set [AugmentedTerminal t])
useBefore nt = do
  ctx <- get
  return $ before ctx nt

-- | Creates the initial LLP context to be used the LLP collection creation.
initLlpContext ::
  (Ord nt, Ord t, Show nt, Show t) =>
  Int ->
  Int ->
  Grammar nt t ->
  Either String (LlpContext nt t)
initLlpContext q k grammar
  | q < 1 = Left "Error: Lookback must be positive. The parser generator should allow for nonnegative lookback but this will be fixed at some point." -- nonnegative."
  | otherwise = do
  ll_table <- evalState (llTableM k augmented_grammar followFunction) first_ctx
  return $
    LlpContext
      { lookback = q,
        lookahead = k,
        theGrammar = augmented_grammar,
        firstContext = first_ctx,
        lastContext = last_ctx,
        before = beforeFunction,
        follow = followFunction,
        table = ll_table
      }
  where
    augmented_grammar = augmentGrammar grammar
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
        suffix = _ui,
        prefix = vi,
        llpConfig = delta
      } = old_item

    result = do
      before_result <- useBefore y
      let before_symbols = toList $ fmap Terminal `Set.map` before_result
      let alpha = if null alpha_x then [] else List.init alpha_x
      uj <- Set.unions <$> mapM (useLast . (++ alpha)) before_symbols
      let x = [List.last alpha_x | not (List.null alpha_x)]
      vj <- useFirst . (x ++) $ fmap Terminal vi
      let x_delta = x ++ fst3 delta
      let x_beta = x ++ beta
      let new_dot_production = DotProduction y alpha x_beta
      let newItem' u v = newItem u v new_dot_production x_delta
      let products' = [(u, v) | u <- toList uj, v <- toList vj]
      if null x
        then return Set.empty
        else Set.fromList <$> mapM (uncurry newItem') products'

    newItem u v new_dot_production x_delta = do
      shortest_prefix <- solveShortestsPrefix v x_delta
      return
        Item
          { dotProduction = new_dot_production,
            suffix = u,
            prefix = v,
            llpConfig = fromJust shortest_prefix
          }

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
      { dotProduction = DotProduction _ alpha_y _,
        suffix = _,
        prefix = v,
        llpConfig = gamma
      } = old_item

    newItem delta u =
      Item
        { dotProduction = DotProduction y delta [],
          suffix = u,
          prefix = v,
          llpConfig = gamma
        }

    y = unpackNonterminal $ List.last alpha_y

    newItems delta = do
      before_result <- useBefore y
      let before_symbols = toList $ fmap Terminal `Set.map` before_result
      u' <- mapM (useLast . (++ delta)) before_symbols
      return $ newItem delta `Set.map` Set.unions u'

    result = do
      ctx <- get
      let grammar = theGrammar ctx
      let deltas = symbols <$> findProductions grammar y
      Set.unions <$> mapM newItems deltas

-- | This function performs step 3 (b) of algorithm 8 in the LLP paper. This is
-- with memoization.
extraLlpItemsMemo ::
  (Ord nt, Show nt, Show t, Ord t, NFData t, NFData nt) =>
  Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)) ->
  State (LlpContext nt t) (Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)))
extraLlpItemsMemo = fixedPointIterate'
  where
    fixedPointIterate' items = do
      new_items <- addExtraItems items
      if items == new_items
        then return new_items
        else fixedPointIterate' new_items

    addExtraItems items = do
      extras <- mapM extraLlpItemMemo $ toList items
      return $ items `Set.union` Set.unions extras

-- | This function computes a single iteration of step 3 for a single item in
-- algorithm 8 of the LLP paper. This will create a subset of a item set. This
-- is done using memoization.
solveLlpItemMemo ::
  (Ord nt, Show nt, Show t, Ord t, NFData t, NFData nt) =>
  Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)) ->
  State (LlpContext nt t) (Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)))
solveLlpItemMemo items = do
  new_items <- mapM newLlpItemsMemo $ toList items
  extraLlpItemsMemo $ Set.unions new_items

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
        x = head x'
        x' = auxiliary a

-- | Creates the LLP collection as described in algorithm 8 from the LLP paper.
-- This is done using memoization.
llpCollectionMemo ::
  (Ord t, Ord nt, Show nt, Show t, NFData t, NFData nt) =>
  State
    (LlpContext nt t)
    ( Either
        String
        ( Map
            ([AugmentedTerminal t], [AugmentedTerminal t])
            ( [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)],
              [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)],
              [Int]
            ),
          Set (Set (Item (AugmentedNonterminal nt) (AugmentedTerminal t)))
        )
    )
llpCollectionMemo = do
  ctx <- get
  let grammar = theGrammar ctx
  let q = lookback ctx
  fmap (BI.second removeNull) <$> auxiliary Map.empty Set.empty [initD q grammar]
  where
    toTuple
      Item
        { dotProduction = (DotProduction _ alpha_z _),
          suffix = x,
          prefix = y,
          llpConfig = gamma
        }
        | null alpha_z = Nothing
        | isTerminal z = Just ((x, y), gamma)
        | otherwise = Nothing
        where
          z = List.last alpha_z

    toTuples = mapMaybe toTuple . Set.toList

    tryInsert q k m (key, a)
      | key `Map.notMember` m = Right new_map
      | a' == a = Right new_map
      | otherwise = Left [i|The grammar is not LLP(#{q}, #{k}) due to the admissible pair #{key_str} could result in the initial pushdown store #{a_str} and #{a_str'}.|]
      where
        a' = m Map.! key
        new_map = Map.insert key a m

        a_str' = concatString $ fst3 a'
        a_str = concatString $ fst3 a
        key_str = (\(x, y) -> "(" ++ x ++ ", " ++ y ++ ")") $ both concatString key
        
        concatString :: (Show m) => [m] -> String
        concatString = unwords . fmap show

    tryInserts q k m = foldM (tryInsert q k) m . toTuples

    removeNull = Set.filter (not . null)

    auxiliary table' items [] = return $ return (table', items)
    auxiliary visited items (top : queue) = do
      next_items <- solveLlpItemsMemo top
      ctx <- get
      let q = lookback ctx
      let k = lookahead ctx
      let new_items = Set.insert top items
      let new_queue = queue ++ toList next_items
      let either_new_visited = tryInserts q k visited top
      if top `Set.member` items
        then auxiliary visited items queue
        else case either_new_visited of
          Left result -> return $ Left result
          Right new_visited -> auxiliary new_visited new_items new_queue

-- | Creates the PSLS table as described in algorithm 9 in the LLP paper.
psls ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Set (Set (Item nt t)) ->
  Map ([t], [t]) (Set ([Symbol nt t], [Symbol nt t], [Int]))
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
            llpConfig = gamma
          } = old_item
        z = List.last alpha_z

-- | Performance the parsing described in step 2 of algorithm 13 of the LLP
-- paper.
parseLlpConfig ::
  (Ord nt, Ord t, Show nt, Show t) =>
  Map (nt, [t]) Int ->
  Int ->
  Grammar nt t ->
  [t] ->
  [Symbol nt t] ->
  Maybe ([Symbol nt t], [Symbol nt t], [Int])
parseLlpConfig table' k grammar y' alpha = f <$> parse (y', alpha, [])
  where
    f (_epsilon, omega, pi') = (alpha, omega, pi')
    production_map = Map.fromList . zip [0 ..] $ productions grammar
    parse (x : _xs, (Terminal y) : ys, parsed)
      | x == y = Just ([], ys, reverse parsed)
      | otherwise = Nothing
    parse (input, (Nonterminal y) : ys, parsed) = do
      index <- (y, take k input) `Map.lookup` table'
      production <- index `Map.lookup` production_map
      parse (input, symbols production ++ ys, index : parsed)
    parse ([], [], parsed) = Just ([], [], reverse parsed)
    parse (_, _, _) = Nothing

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
  let Production _ s = head $ findProductions grammar start'
  zero_symbols <- toList <$> useFirst s
  let zero_keys = Map.fromList $ (,([Nonterminal start'], tail s, [0])) . ([],) <$> zero_symbols
  return zero_keys

llpParserTableWithStarts ::
  (Ord nt, Ord t, Show nt, Show t, NFData t, NFData nt) =>
  Int ->
  Int ->
  Grammar nt t ->
  Either
    String
    ( Map
        ([AugmentedTerminal t], [AugmentedTerminal t])
        ( [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)],
          [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)],
          [Int]
        )
    )
llpParserTableWithStarts q k grammar = do
  init_context <- initLlpContext q k grammar
  let (either_table, starts) = tableAndStarts init_context
  _table <- either_table
  return $ starts `Map.union` _table
  where
    tableAndStarts ctx = flip evalState ctx $ do
      either_table' <- llpParserTable
      starts' <- allStarts
      return (either_table', starts')

data Bracket a = LBracket a | RBracket a deriving (Eq, Ord, Show, Functor)

llpParserTableWithStartsHomomorphisms ::
  (NFData t, NFData nt, Ord nt, Show nt, Show t, Ord t) =>
  Int ->
  Int ->
  Grammar nt t ->
  Either
    String
    ( Map
        ([AugmentedTerminal t], [AugmentedTerminal t])
        ( [Bracket (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t))],
          [Int]
        )
    )
llpParserTableWithStartsHomomorphisms q k grammar = do
  init_context <- initLlpContext q k grammar
  let (either_table', starts') = tableAndStarts init_context
  table' <- either_table'
  return $ table' `Map.union` starts'
  where
    tableAndStarts ctx = flip evalState ctx $ do
      either_table' <- llpParserTable
      starts' <- allStarts
      return (fmap homomorphisms <$> either_table', start_homomorphisms <$> starts')
    start_homomorphisms (_, omega, pi') = (LBracket <$> reverse omega, pi')
    homomorphisms (alpha, omega, pi') = (right ++ left, pi')
      where
        left = LBracket <$> reverse omega
        right = RBracket <$> alpha

-- | Creates the LLP parsing table if Nothing is returned then the table could
-- not be created since the grammar is not LLP(q,k).
llpParserTable ::
  (Ord nt, Ord t, Show nt, Show t, NFData t, NFData nt) =>
  State
    (LlpContext nt t)
    ( Either
        String
        ( Map
            ([AugmentedTerminal t], [AugmentedTerminal t])
            ([Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)], [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)], [Int])
        )
    )
llpParserTable = do
  either_tuple <- llpCollectionMemo
  return $ do
    (llp_table, _) <- either_tuple
    return llp_table

-- | Given a lsit create all the pairs with q lookback and k lookahead which
-- will be used as keys in the table.
pairLookup :: (Ord t, Show t) => Map ([t], [t]) v -> Int -> Int -> [t] -> [Maybe v]
pairLookup table' q k = toList . auxiliary Seq.empty Seq.empty . Seq.fromList
  where
    auxiliary es _ Empty = es
    auxiliary es ys (x :<| xs) = auxiliary (es :|> val) (ys :|> x) xs
      where
        backwards = takeR q ys
        forwards = Seq.take (k - 1) xs
        val = (toList backwards, toList $ x <| forwards) `Map.lookup` table'
        takeR i' seq' = Seq.drop (n - i') seq'
          where
            n = Seq.length seq'

-- | glues two stacks together as described in definition 1 of the LLP paper.
glue ::
  (Eq nt, Eq t) =>
  ([Symbol nt t], [Symbol nt t], [Int]) ->
  ([Symbol nt t], [Symbol nt t], [Int]) ->
  Maybe ([Symbol nt t], [Symbol nt t], [Int])
glue (alpha_l, omega_l, pi_l) (alpha_r, omega_r, pi_r)
  | omega_l == alpha_r = Just (alpha_l, omega_r, pi')
  | omega_l `List.isPrefixOf` alpha_r = Just (alpha_l ++ beta_0, omega_r, pi')
  | alpha_r `List.isPrefixOf` omega_l = Just (alpha_l, omega_r ++ beta_1, pi')
  | otherwise = Nothing
  where
    pi' = pi_l ++ pi_r
    beta_0 = drop (length omega_l) alpha_r
    beta_1 = drop (length alpha_r) omega_l

-- | Given a grammar it will parse the string using a LLP table.
llpParse ::
  (Ord nt, Show nt, Show t, Ord t, NFData t, NFData nt) =>
  Int ->
  Int ->
  Grammar nt t ->
  [t] ->
  Either String [Int]
llpParse q k grammar string = do
  table' <- llpParserTableWithStarts q k grammar
  let padded_string = addStoppers $ aug string
  pairs <- maybeToEither "Could not be parsed." . sequence $ pairLookup table' q k padded_string
  thd3 <$> maybeToEither "Could not be parsed." (glueAll pairs)
  where
    addStoppers = (replicate 1 RightTurnstile ++) . (++ replicate 1 LeftTurnstile)
    aug = fmap AugmentedTerminal
    glueAll [] = Nothing
    glueAll (x : xs) = foldM glue x xs
