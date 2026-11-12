module Alpacc.Parser.LLP
  ( Item (..),
    DotProduction (..),
    llpParserTableWithStarts,
    llpParse,
    llpParserTableWithStartsHomomorphisms,
    Bracket (..),
    LlpContext (..),
    FlatTree (..),
    PushCounts (..),
    llpParseFlatTree,
    llpParseDirectPush,
    psls,
  )
where

import Alpacc.Grammar
import Alpacc.Parser.LL hiding (before, follow, llParse)
import Control.DeepSeq
import Control.Monad (foldM, join)
import Data.IORef
import Control.Monad.State
import Data.Array (listArray, (!))
import Data.Bifunctor qualified as BI
import Data.Foldable
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Maybe
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq hiding (Seq (..), (><), (|>))
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tuple.Extra (both, fst3, thd3)
import Data.Word
import GHC.Generics
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
initD q grammar =
  case findProductions grammar (start grammar) of
    [p] -> Set.map (init' p) $ last q grammar $ symbols p
    _any -> Set.empty
  where
    init' p l =
      Item
        { dotProduction = toDotProduction p,
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
  Grammar (AugmentedNonterminal nt) (AugmentedTerminal t) ->
  Either Text (LlpContext nt t)
initLlpContext q k augmented_grammar
  | q < 0 = Left "Error: Lookback must be non-negative."
  | k < 1 = Left "Error: Lookahead must be positive."
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
    groupByDotSymbol symbol_map (a : as) =
      case auxiliary a of
        [] -> groupByDotSymbol symbol_map as
        x : _ | x `Map.member` symbol_map -> groupByDotSymbol (new_symbol_map x) as
        x : _ -> groupByDotSymbol (new_symbol_map' x) as
      where
        new_symbol_map y = Map.adjust (Set.insert a) y symbol_map
        new_symbol_map' y = Map.insert y (Set.singleton a) symbol_map

-- | Creates the LLP collection as described in algorithm 8 from the LLP paper.
-- This is done using memoization.
llpCollectionMemo ::
  (Ord t, Ord nt, Show nt, Show t, NFData t, NFData nt) =>
  State
    (LlpContext nt t)
    ( Either
        Text
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
      | otherwise =
          Left $
            Text.pack [i|The grammar is not LLP(#{q}, #{k}) due to the admissible pair #{key_str} could result in the initial pushdown store #{a_str} and #{a_str'}.|]
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
  let (s, tail_s) =
        case findProductions grammar start' of
          Production _ s'@(_ : tail_s') : _ -> (s', tail_s')
          _any -> ([], [])
  zero_symbols <- toList <$> useFirst s
  let zero_keys = Map.fromList $ (,([Nonterminal start'], tail_s, [])) . ([],) <$> zero_symbols
  return zero_keys

llpParserTableWithStarts ::
  (Ord nt, Ord t, Show nt, Show t, NFData t, NFData nt) =>
  Int ->
  Int ->
  Grammar (AugmentedNonterminal nt) (AugmentedTerminal t) ->
  Either
    Text
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
  Grammar (AugmentedNonterminal nt) (AugmentedTerminal t) ->
  Either
    Text
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
        Text
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

-- | Given a list create all the pairs with q lookback and k lookahead which
-- will be used as keys in the table.
-- Uses a bounded deque (list of length <= q) for lookback, avoiding O(n) Seq.
pairLookup :: (Ord t, Show t) => Map ([t], [t]) v -> Int -> Int -> [t] -> [Maybe v]
pairLookup table' q k = go []
  where
    go _ [] = []
    go back (x : xs) =
      val : go back' xs
      where
        back'   = keepQ (back ++ [x])
        forward = take (k - 1) xs
        val     = Map.lookup (back, x : forward) table'
    keepQ xs = drop (length xs - q) xs

-- | Checks if the first sequence is a prefix of the second.
seqIsPrefixOf :: (Eq a) => Seq a -> Seq a -> Bool
seqIsPrefixOf xs ys = Seq.take (Seq.length xs) ys == xs

-- | glues two stacks together as described in definition 1 of the LLP paper.
glue ::
  (Eq nt, Eq t) =>
  (Seq (Symbol nt t), Seq (Symbol nt t), Seq Int) ->
  (Seq (Symbol nt t), Seq (Symbol nt t), Seq Int) ->
  Maybe (Seq (Symbol nt t), Seq (Symbol nt t), Seq Int)
glue (alpha_l, omega_l, pi_l) (alpha_r, omega_r, pi_r)
  | omega_l == alpha_r = Just (alpha_l, omega_r, pi')
  | omega_l `seqIsPrefixOf` alpha_r = Just (alpha_l <> beta_0, omega_r, pi')
  | alpha_r `seqIsPrefixOf` omega_l = Just (alpha_l, omega_r <> beta_1, pi')
  | otherwise = Nothing
  where
    pi' = pi_l <> pi_r
    beta_0 = Seq.drop (Seq.length omega_l) alpha_r
    beta_1 = Seq.drop (Seq.length alpha_r) omega_l

-- | Given a grammar it will parse the string using a LLP table.
llpParse ::
  (Ord nt, Show nt, Show t, Ord t, NFData t, NFData nt) =>
  Int ->
  Int ->
  Map
    ([AugmentedTerminal t], [AugmentedTerminal t])
    ( [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)],
      [Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)],
      [Int]
    ) ->
  [t] ->
  Maybe [Int]
llpParse q k table string = do
  let padded_string = addStoppers $ aug string
  pairs <- sequence $ pairLookup table q k padded_string
  toList . thd3 <$> glueAll (toSeqTuple <$> pairs)
  where
    addStoppers = ([RightTurnstile] <>) . (<> [LeftTurnstile])
    aug = fmap AugmentedTerminal
    toSeqTuple (a, b, c) = (Seq.fromList a, Seq.fromList b, Seq.fromList c)
    glueAll [] = Nothing
    glueAll (x : xs) = foldM glue x xs

-- | A parse tree in compact flat form: the tree contains only production
-- nodes (terminal slots of the derivation are dropped), in preorder.
-- Parents are indices into that preorder; the root's parent is 0.  Tokens
-- attach to the tree through 'flatTokenParents', one parent index per token
-- in token order.
data FlatTree i = FlatTree
  { flatProductions :: !(Seq i),
    flatParents :: !(Seq i),
    flatTokenParents :: !(Seq i)
  }
  deriving (Show, Ord, Eq)

instance Semigroup (FlatTree i) where
  FlatTree a b c <> FlatTree a' b' c' = FlatTree (a <> a') (b <> b') (c <> c')

instance Monoid (FlatTree i) where
  mempty = FlatTree Seq.empty Seq.empty Seq.empty

data CST i m t
  = CSTProduction i [CST i m t]
  | CSTTerminal m t
  deriving (Show, Ord, Eq)

instance Functor (CST i m) where
  fmap f (CSTProduction j subtrees) = CSTProduction j (map (fmap f) subtrees)
  fmap f (CSTTerminal m t) = CSTTerminal m (f t)

llpParseTree ::
  (Ord nt, Show nt, Show t, Ord t, Show t', Ord t', NFData t', NFData t, NFData nt, Show m) =>
  Grammar
    (AugmentedNonterminal (Symbol nt t))
    (AugmentedTerminal t') ->
  Int ->
  Int ->
  Map
    ([AugmentedTerminal t'], [AugmentedTerminal t'])
    ( [Symbol (AugmentedNonterminal (Symbol nt t)) (AugmentedTerminal t')],
      [Symbol (AugmentedNonterminal (Symbol nt t)) (AugmentedTerminal t')],
      [Int]
    ) ->
  [(t', m)] ->
  Maybe (CST Word64 m t)
llpParseTree grammar q k table str_meta = do
  derivation <- llpParse q k table str
  let (tree, _, _) = mkTree meta derivation
  pure tree
  where
    (str, meta) = unzip str_meta
    prod_map = Map.fromList $ zip [(0 :: Int) ..] $ productions grammar
    toProd = flip Map.lookup prod_map

    mkTree [] [d]
      | Just (Production (AugmentedNonterminal (Nonterminal _)) []) <- toProd d =
          (CSTProduction (fromIntegral d :: Word64) [], [], [])
    mkTree [m] [d]
      | Just (Production (AugmentedNonterminal (Terminal t)) [_]) <- toProd d =
          (CSTTerminal m t, [], [])
    mkTree ms@(m : tms) (d : ds)
      | Just (Production (AugmentedNonterminal (Terminal t)) _) <- toProd d =
          (CSTTerminal m t, tms, ds)
      | Just (Production (AugmentedNonterminal (Nonterminal _)) syms) <- toProd d =
          let (nodes, new_ms, new_ds) = foldl' auxiliary ([], ms, ds) syms
           in (CSTProduction (fromIntegral d :: Word64) nodes, new_ms, new_ds)
    mkTree [] (d : ds)
      | Just (Production (AugmentedNonterminal (Nonterminal _)) syms) <- toProd d =
          let (nodes, new_ms, new_ds) = foldl' auxiliary ([], [], ds) syms
           in (CSTProduction (fromIntegral d :: Word64) nodes, new_ms, new_ds)
    mkTree _ _ = error "An internal error occurred."

    auxiliary (ns', ms', ds') _ = (ns' <> [n], ms'', ds'')
      where
        (n, ms'', ds'') = mkTree ms' ds'

flattenTree :: (Integral i) => CST i m t -> FlatTree i
flattenTree = fst . mkFlatTree 0 0
  where
    mkFlatTree c p (CSTTerminal _ _) =
      (mempty {flatTokenParents = Seq.singleton p}, c)
    mkFlatTree c p (CSTProduction j subtrees) =
      foldl' auxiliary (curr, succ c) subtrees
      where
        curr =
          FlatTree
            { flatProductions = Seq.singleton j,
              flatParents = Seq.singleton p,
              flatTokenParents = Seq.empty
            }
        auxiliary (flat, c') t = (flat <> flat', c'')
          where
            (flat', c'') = mkFlatTree c' c t

llpParseFlatTree ::
  (Ord nt, Show nt, Show t, Ord t, Show t', Ord t', NFData t', NFData t, NFData nt, Show m) =>
  Grammar
    (AugmentedNonterminal (Symbol nt t))
    (AugmentedTerminal t') ->
  Int ->
  Int ->
  Map
    ([AugmentedTerminal t'], [AugmentedTerminal t'])
    ( [Symbol (AugmentedNonterminal (Symbol nt t)) (AugmentedTerminal t')],
      [Symbol (AugmentedNonterminal (Symbol nt t)) (AugmentedTerminal t')],
      [Int]
    ) ->
  [(t', m)] ->
  Maybe (FlatTree Word64)
llpParseFlatTree grammar q k table str_meta =
  flattenTree <$> llpParseTree grammar q k table str_meta

-- | Internal state for 'llpParseDirectPush'.
data PushSt tok bsym meta = PushSt
  { psStartup :: !Bool
  , psBack    :: ![(tok, meta)]
  , psAhead   :: ![(tok, meta)]
  , psBstk    :: ![bsym]
  , psPstk    :: ![(Int, Int)]
  , psTreeCount  :: !Word64
  , psTokenCount :: !Word64
  , psTreeIdx    :: !Int
  }

-- | Final counts reported by 'llpParseDirectPush'.
data PushCounts = PushCounts
  { pushTreeNodes :: !Word64
  , pushTokenParents :: !Word64
  }

-- | Push-mode streaming parser.  Emits the compact flat tree (see 'FlatTree'):
-- production nodes via @emitProduction id parent@ in preorder (compact
-- indices) and one @emitTokenParent parent@ per token in token order.
-- Returns @(step, finalize)@:
--   * @step (tok, meta)@ -- call once per token in order; after a parse error
--     further calls are no-ops.
--   * @finalize@ -- call once after all tokens to process the end sentinel and
--     get the final counts (Nothing = parse error).
--
-- No intermediate token list is allocated; this is meant to be driven by
-- 'tokenizeWithBS' in a single pass.
llpParseDirectPush ::
  (Ord nt, Eq nt, Show nt, Ord t, Eq t, Show t, NFData t, NFData nt) =>
  Grammar
    (AugmentedNonterminal (Symbol nt T))
    (AugmentedTerminal (Unused t)) ->
  Int ->
  Int ->
  Map
    ([AugmentedTerminal (Unused t)], [AugmentedTerminal (Unused t)])
    ( [Bracket (Symbol (AugmentedNonterminal (Symbol nt T)) (AugmentedTerminal (Unused t)))],
      [Int]
    ) ->
  (Word64 -> Word64 -> IO ()) ->
  (Word64 -> IO ()) ->
  IO
    ( (AugmentedTerminal (Unused t), meta) -> IO ()
    , IO (Maybe PushCounts)
    )
llpParseDirectPush grammar q k table emitProduction emitTokenParent = do
  -- Parser state record.  'startup=True' while we accumulate the first k tokens
  -- needed to look up the RightTurnstile position in the table.
  let initSt = PushSt
        { psStartup   = True
        , psBack      = [] :: [(AugmentedTerminal (Unused t), meta)]
        , psAhead     = [] :: [(AugmentedTerminal (Unused t), meta)]
        , psBstk      = [] :: [Symbol (AugmentedNonterminal (Symbol nt T)) (AugmentedTerminal (Unused t))]
        , psPstk      = [] :: [(Int, Int)]
        , psTreeCount  = 0 :: Word64
        , psTokenCount = 0 :: Word64
        , psTreeIdx    = 0 :: Int
        }
  stateRef <- newIORef (Right initSt)

  let step tok = do
        est <- readIORef stateRef
        case est of
          Left _   -> pure ()
          Right st ->
            if psStartup st
              then do
                let ahead' = psAhead st ++ [tok]
                if length ahead' < k
                  then writeIORef stateRef $ Right st { psAhead = ahead' }
                  else do
                    -- Have k tokens: process RightTurnstile position, then drain.
                    let fwd = map fst (take (k - 1) ahead')
                        startKey = ([] :: [AugmentedTerminal (Unused t)], RightTurnstile : fwd)
                    r <- processStart st ahead' startKey
                    case r of
                      Nothing  -> writeIORef stateRef (Left ())
                      Just st' -> writeIORef stateRef (Right st')
              else do
                let ahead' = psAhead st ++ [tok]
                r <- drain st { psAhead = ahead' }
                case r of
                  Nothing  -> writeIORef stateRef (Left ())
                  Just st' -> writeIORef stateRef (Right st')

      -- Compute forward-lookahead tokens for drainAll, mirroring pairLookup on the
      -- padded string: exactly one LeftTurnstile sentinel follows the last real token.
      padFwd toks = take (k - 1) (map fst toks ++ [LeftTurnstile])

      finalize = do
        est <- readIORef stateRef
        case est of
          Left _   -> pure Nothing
          Right st ->
            if psStartup st
              then do
                -- Fewer than k tokens total: process RightTurnstile then drain all.
                let fwd      = padFwd (psAhead st)
                    startKey = ([] :: [AugmentedTerminal (Unused t)], RightTurnstile : fwd)
                r <- processStart st (psAhead st) startKey
                case r of
                  Nothing  -> pure Nothing
                  Just st' -> finishDrain st'
              else finishDrain st

      -- Process the RightTurnstile position, then run drain.
      processStart st ahead startKey =
        case Map.lookup startKey table of
          Nothing -> pure Nothing
          Just (brackets, prods) ->
            case pshBrackets brackets (psBstk st) of
              Nothing    -> pure Nothing
              Just bstk1 -> do
                (cp1, ct1, tidx1, _, pstk1) <-
                  emtProds prods [] (psTreeCount st) (psTokenCount st) (psTreeIdx st) (psPstk st)
                let back1 = keepQ (psBack st ++ [(RightTurnstile, error "no meta")])
                drain st { psStartup = False, psBack = back1, psAhead = ahead
                         , psBstk = bstk1, psPstk = pstk1
                         , psTreeCount = cp1, psTokenCount = ct1, psTreeIdx = tidx1 }

      -- drain: consume tokens while ahead has more than k-1 entries.
      drain st
        | length (psAhead st) > k - 1 = case psAhead st of
            [] -> pure (Just st)
            (current : rest) -> do
              let fwdToks = map fst (take (k - 1) rest)
              r <- processOne current fwdToks st
              case r of
                Nothing  -> pure Nothing
                Just st' -> drain st' { psAhead = rest }
        | otherwise = pure (Just st)

      -- finishDrain: drainAll remaining tokens then process LeftTurnstile.
      finishDrain st = do
        r <- drainAll st
        case r of
          Nothing  -> pure Nothing
          Just st' -> processEnd st'

      -- drainAll: process every remaining token, padding forward with LeftTurnstile.
      drainAll st = case psAhead st of
        [] -> pure (Just st)
        (current : rest) -> do
          let fwdToks = padFwd rest
          r <- processOne current fwdToks st
          case r of
            Nothing  -> pure Nothing
            Just st' -> drainAll st' { psAhead = rest }

      -- processOne: table lookup + bracket check + emit productions for one token.
      processOne current fwdToks st = do
        let backToks   = map fst (psBack st)
            currentTok = fst current
            key        = (backToks, currentTok : fwdToks)
        case Map.lookup key table of
          Nothing -> pure Nothing
          Just (brackets, prods) ->
            case pshBrackets brackets (psBstk st) of
              Nothing    -> pure Nothing
              Just bstk' -> do
                (cp', ct', tidx', _, pstk') <-
                  emtProds prods [snd current] (psTreeCount st) (psTokenCount st) (psTreeIdx st) (psPstk st)
                pure $ Just st { psBack = keepQ (psBack st ++ [current])
                               , psBstk = bstk', psPstk = pstk'
                               , psTreeCount = cp', psTokenCount = ct', psTreeIdx = tidx' }

      processEnd st = do
        let backToks = map fst (psBack st)
            key      = (backToks, [LeftTurnstile])
        case Map.lookup key table of
          Nothing -> pure Nothing
          Just (brackets, prods) ->
            case pshBrackets brackets (psBstk st) of
              Nothing    -> pure Nothing
              Just bstk' -> do
                (cp', ct', _, _, _) <-
                  emtProds prods [] (psTreeCount st) (psTokenCount st) (psTreeIdx st) (psPstk st)
                if null bstk'
                  then pure (Just PushCounts { pushTreeNodes = cp', pushTokenParents = ct' })
                  else pure Nothing

  pure (step, finalize)
  where
    prod_map = Map.fromList $ zip [0..] $ productions grammar
    prod_arr = listArray (0, Map.size prod_map - 1) $ productions grammar

    isTerminalProd d = case Map.lookup d prod_map of
      Just (Production (AugmentedNonterminal (Terminal t)) _) -> Just t
      _ -> Nothing

    arityOf d = length $ filter isNt $ symbols (prod_arr ! d)
      where
        isNt (Nonterminal _) = True
        isNt _               = False

    keepQ xs = drop (length xs - q) xs

    pshBrackets [] stk = Just stk
    pshBrackets (LBracket x : rest) stk = pshBrackets rest (x : stk)
    pshBrackets (RBracket x : rest) (top : stk)
      | x == top  = pshBrackets rest stk
      | otherwise = Nothing
    pshBrackets (RBracket _ : _) [] = Nothing

    emtProds [] ms !cp !ct !tidx pstk = pure (cp, ct, tidx, ms, pstk)
    emtProds (d : ds) ms !cp !ct !tidx pstk = do
      let arity  = arityOf d
          parent = case pstk of { (p, _) : _ -> fromIntegral p; [] -> 0 }
          pstk'  = case pstk of
                     (_, 1) : t   -> t
                     (p', n) : t  -> (p', n - 1) : t
                     []           -> []
          pstk2  = if arity > 0 then (tidx, arity) : pstk' else pstk'
      case isTerminalProd d of
        Just _ -> case ms of
          (_ : ms') -> do
            emitTokenParent parent
            emtProds ds ms' cp (ct + 1) tidx pstk2
          [] -> emtProds ds [] cp ct tidx pstk2
        Nothing -> do
          emitProduction (fromIntegral d) parent
          emtProds ds ms (cp + 1) ct (tidx + 1) pstk2
