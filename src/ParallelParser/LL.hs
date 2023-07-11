module ParallelParser.LL
  ( nullableOne,
    nullable,
    first,
    follow,
    last,
    before,
    fixedPointIterate,
    llTable,
    llParse,
    leftmostDerivations,
    leftmostDerive,
    derivations,
    naiveFirst,
    naiveFollow,
    alphaBeta,
    firstMemoized,
    lastMemoized,
    initFirstMemoizedContext,
    initLastMemoizedContext,
    AlphaBetaMemoizedContext (..),
    derivableNLengths,
    nonderivableNLengths,
    validLlSubstrings,
    firstAndFollow,
    lastAndBefore,
    firstMap,
    closureAlgorithm,
    leftFactorNonterminals,
    isLeftRecursive,
    truncatedProduct,
    llTableM
  )
where

import Control.Monad.State hiding (state)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (dupe, both)
import ParallelParser.Grammar
import Prelude hiding (last)
import Data.Sequence (Seq (..), (<|), (><))
import qualified Data.Sequence as Seq hiding (Seq (..), (<|), (><), (|>))
import Data.Foldable
import qualified Data.Bifunctor as Bifunctor
import Data.Composition
import Control.DeepSeq
import GHC.Generics

-- import Debug.Trace (traceShow)
-- debug x = traceShow x x

listProducts :: Int -> [a] -> [[a]]
listProducts = snd .: auxiliary
  where
    auxiliary 1 zs = (result, result)
      where
        result = map (:[]) zs
    auxiliary n zs = (next, result ++ next)
      where
        (prev, result) = auxiliary (n - 1) zs
        next = [x:y | x <- zs, y <- prev]

validLlSubstrings :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Set [t]
validLlSubstrings k = Set.unions . firstMap k . substringGrammar

derivableNLengths :: (Ord t, Ord nt, Show nt, Show t) => Int -> Grammar nt t -> Set [t]
derivableNLengths n grammar =
  Set.fromList
    . bfs Set.empty
    . Seq.singleton
    . List.singleton
    . Nonterminal
    $ start grammar
  where
    derivations' = derivations grammar
    bfs _ Empty = []
    bfs visited (top :<| queue)
      | n_terms `Set.member` visited = bfs visited queue
      | all isTerminal top = (unpackTerminal <$> top) : bfs visited' (queue >< derivations' top)
      | otherwise = bfs visited' (queue >< derivations' top)
      where
        n_terms = take (n + 1) top
        visited' = Set.insert n_terms visited

nonderivableNLengths :: (Ord nt, Show nt, Show t, Ord t) => Int -> Grammar nt t -> Set [t]
nonderivableNLengths n grammar = nonderivable
  where
    ts = terminals grammar
    combs = Set.fromList $ listProducts n ts
    derivable = derivableNLengths (n + 1) grammar
    nonderivable = combs `Set.difference` derivable

-- | Given a production it produces a list of tuples where the first element is
-- the left hand side of the production. The second element is a tuple where the
-- first element is a nonterminal found in the production and the second element
-- is symbols following that nonterminal. 
rightProductons :: Production nt t -> [(nt, (nt, [Symbol nt t]))]
rightProductons (Production aj symbols') = (aj,) <$> rightSymbols symbols'

-- | Given a string of symbols create all the leftmost derivations.
leftmostDerive ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Grammar nt t ->
  [Symbol nt t] ->
  Seq [Symbol nt t]
leftmostDerive grammar = derive Seq.empty . Seq.fromList
  where
    toSequences = fmap (fmap Seq.fromList)
    production_map = toSequences . toProductionsMap $ productions grammar
    derive _ Empty = Empty
    derive ys ((Terminal x) :<| xs) = derive (ys :|> Terminal x) xs
    derive ys ((Nonterminal x) :<| xs) = ps
      where
        smallDerive e = ys >< e >< xs
        ps = Seq.fromList $ toList . smallDerive <$> (production_map Map.! x)

-- | Given a string of symbols create all derivations.
derivations ::
  (Ord t, Ord nt, Show nt, Show t) =>
  Grammar nt t ->
  [Symbol nt t] ->
  Seq [Symbol nt t]
derivations grammar str = (str <|) .  derive Seq.empty Seq.empty $ Seq.fromList str
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

-- | Naïvely creates the first sets for a given string of symbols. This is done
-- using breadth first search.
naiveFirst :: (Show nt, Show t, Ord nt, Ord t) => Int -> Grammar nt t -> [Symbol nt t] -> Set [t]
naiveFirst k grammar = Set.fromList . bfs Set.empty . Seq.singleton
  where
    derivations' = derivations grammar
    bfs _ Empty = []
    bfs visited (top :<| queue)
      | k_plus_one_terms `Set.member` visited = bfs visited queue
      | all isTerminal k_terms = (unpackTerminal <$> k_terms) : bfs new_visited (queue >< derivations' top)
      | otherwise = bfs new_visited (queue >< derivations' top)
      where
        k_terms = take k top
        k_plus_one_terms = take (k + 1) top
        new_visited = Set.insert k_plus_one_terms visited

-- | Naïvely creates creates all leftmost derivations which results in
-- terminals.
leftmostDerivations :: (Show nt, Show t, Ord nt, Ord t) => Int -> Grammar nt t -> [Symbol nt t] -> Set [t]
leftmostDerivations k grammar = Set.map (take k) . bfs Set.empty Set.empty . Seq.singleton
  where
    leftmostDerive' = leftmostDerive grammar
    bfs set _ Empty = set
    bfs set visited (top :<| queue)
      | k_terms `Set.member` visited = bfs set visited queue
      | all isTerminal k_terms = bfs new_set new_visited (queue >< leftmostDerive' top)
      | otherwise = bfs set new_visited (queue >< leftmostDerive' top)
      where
        new_set = Set.insert (unpackTerminal <$> k_terms) set
        k_terms = take (k + 1) top
        new_visited = Set.insert k_terms visited

-- | Naïvely creates the follow sets for a given string of symbols. This is done
-- using breadth first search and applying first and to the symbols after the
-- nonterminals of a given derivation in the bfs.
naiveFollows :: (Show nt, Show t, Ord nt, Ord t) => Int -> Grammar nt t -> Map nt (Set [t])
naiveFollows k grammar =
  Map.unionWith Set.union init_maps
    . Map.unionsWith Set.union
    $ bfs Set.empty start'
  where
    Production nt _ = head $ findProductions grammar (start grammar)
    init_maps = Map.fromList $ (,Set.empty) <$> nonterminals grammar
    first'' = first k grammar
    start' = Seq.singleton [Nonterminal nt]
    derivations' = derivations grammar
    bfs _ Empty = []
    bfs visited (top :<| queue)
      | right_symbols_set `Set.isSubsetOf` visited = bfs visited queue
      | null right_symbols = bfs new_visited queue
      | otherwise = maps ++ bfs new_visited (queue >< derivations' top)
      where
        right_symbols = Bifunctor.second first'' <$> rightSymbols top
        maps = uncurry Map.singleton <$> right_symbols
        right_symbols_set = Set.fromList right_symbols
        new_visited = right_symbols_set `Set.union` visited

-- | The naïve follow set is created and then used as a function.
naiveFollow :: (Show nt, Show t, Ord nt, Ord t) => Int -> Grammar nt t -> nt -> Set [t]
naiveFollow k grammar = (follow_map Map.!)
  where
    follow_map = naiveFollows k grammar

-- | Performs fixed point iteration until a predicate holds true.
fixedPointIterate :: (Show b, Eq b) => (b -> b -> Bool) -> (b -> b) -> b -> b
fixedPointIterate cmp f = fst . head . dropWhile (uncurry cmp) . iterateFunction
  where
    iterateFunction = drop 1 . iterate swapApply . dupe
    swapApply (n, _) = (f n, n)

-- | Determines the nullablity of each nonterminal using the algorithm described
-- in Introduction to Compiler Design.
nullables :: (Show nt, Show t, Ord nt, Ord t) => Grammar nt t -> Map nt Bool
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

-- | Determines the nullablity of each symbol using the algorithm described
-- in Introduction to Compiler Design.
nullableOne :: (Show nt, Show t, Ord nt, Ord t) => Grammar nt t -> Symbol nt t -> Bool
nullableOne _ (Terminal _) = False
nullableOne grammar (Nonterminal nt) = nullable_map Map.! nt
  where
    nullable_map = nullables grammar

-- | Determines the nullablity of a string of symbols using the algorithm
-- described in Introduction to Compiler Design.
nullable :: (Show nt, Show t, Ord nt, Ord t) => Grammar nt t -> [Symbol nt t] -> Bool
nullable grammar = all nullableOne'
  where
    nullableOne' = nullableOne grammar

-- | Concatenates each element of set a on the front of each element of set b
-- and then takes the first k symbols.
truncatedProduct :: Ord t => Int -> Set [t] -> Set [t] -> Set [t]
truncatedProduct k a b = Set.fromList token_product
  where
    token_product = [take k $ ts ++ ts' | ts <- a_list, ts' <- b_list]
    a_list = Set.toList a
    b_list = Set.toList b

-- | The state used for memoization where the map keys are input and the output
-- the values belonging to the key.
type MemoState k v = State (Map.Map k v) v
-- | A function that can be memoized.
type MemoFunction k v = (k -> MemoState k v) -> (k -> MemoState k v)

-- | Given a function that can be memoized create a function that is memoized.
memoize :: Ord k => MemoFunction k v -> k -> MemoState k v
memoize f k = do
  memory <- get
  let lookup_result = Map.lookup k memory
  case lookup_result of
    Just result -> return result
    Nothing -> do
      result <- f (memoize f) k
      put (Map.insert k result memory)
      return result

-- | Given a memoized function a value and a initial state return the result
-- and the resulting state.
runMemoized :: Ord k => MemoFunction k v -> k -> Map.Map k v -> (v, Map.Map k v)
runMemoized f arg = runState (memoize f arg)

-- | Given a string of symbols it creates every way to split the string in two
-- computes the first value of those pairs and performs the truncated product
-- on them. This is a memoized function.
memoAlphaBetaProducts :: (Ord nt, Ord t) => Int -> Map nt (Set [t]) -> MemoFunction [Symbol nt t] (Set [t])
memoAlphaBetaProducts _ _ _ [] = error "Input string cannot be empty."
memoAlphaBetaProducts _ _ _ [Terminal t] = return $ Set.singleton [t]
memoAlphaBetaProducts _ first_map _ [Nonterminal nt] = return $ first_map Map.! nt
memoAlphaBetaProducts k _ self string = Set.unions <$> mapM bothSubProducts alpha_betas
  where
    alpha_betas = alphaBeta string
    bothSubProducts (a, b) = do
      a' <- self a
      b' <- self b
      return $ truncatedProduct k a' b'

-- | Given a string of symbols it creates every way to split the string in two
-- computes the first value of those pairs and performs the truncated product
-- on them.
alphaBetaProducts :: (Ord nt, Ord t) => Int -> Map nt (Set [t]) -> [Symbol nt t] -> Set [t]
alphaBetaProducts _ _ [] = error "Input string cannot be empty."
alphaBetaProducts _ _ [Terminal t] = Set.singleton [t]
alphaBetaProducts _ first_map [Nonterminal nt] = first_map Map.! nt
alphaBetaProducts k first_map string = Set.unions $ map subProducts alpha_betas
  where
    alpha_betas = alphaBeta string
    subProduct = alphaBetaProducts k first_map
    subProducts = uncurry (truncatedProduct k) . both subProduct

-- | Creates a memoized alphaBeta function given a the lookahead and grammar. 
mkMemoAlphaBetaProducts :: (Show nt, Show t, Ord nt, Ord t) => Int -> Grammar nt t -> MemoFunction [Symbol nt t] (Set [t])
mkMemoAlphaBetaProducts k grammar = memoAlphaBetaProducts k first_map
  where
    first_map = firstMap k grammar

-- | A record used to easily contain a memoized alphaBeta function and the
-- current state it is in.
data AlphaBetaMemoizedContext nt t = AlphaBetaMemoizedContext
  { alphaBetaFunction :: MemoFunction [Symbol nt t] (Set [t]),
    alphaBetaState :: Map [Symbol nt t] (Set [t]),
    look :: Int
  } deriving (Generic)

instance (Ord t, Ord nt) => Semigroup (AlphaBetaMemoizedContext nt t) where
  a <> b = a { alphaBetaState = new_state }
    where
      a_state = alphaBetaState a
      b_state = alphaBetaState b
      new_state = Map.unionWith Set.union a_state b_state

instance (NFData t, NFData nt) => NFData (AlphaBetaMemoizedContext nt t)

-- | Creates the initial context used for the memoized first function.
initFirstMemoizedContext :: (Show nt, Show t, Ord nt, Ord t) => Int -> Grammar nt t -> AlphaBetaMemoizedContext nt t
initFirstMemoizedContext k grammar =
  AlphaBetaMemoizedContext {
    alphaBetaFunction = mkMemoAlphaBetaProducts k grammar,
    alphaBetaState = Map.empty,
    look = k
  }

-- | Creates the initial context used for the memoized last function.
initLastMemoizedContext ::
  (Show nt, Show t, Ord nt, Ord t) =>
  Int ->
  Grammar nt t ->
  AlphaBetaMemoizedContext nt t
initLastMemoizedContext q grammar =
  AlphaBetaMemoizedContext {
    alphaBetaFunction = mkMemoAlphaBetaProducts q (reverseGrammar grammar),
    alphaBetaState = Map.empty,
    look = q
  }

-- | Every possible way to split a string in two.
alphaBeta :: [a] -> [([a], [a])]
alphaBeta string
  | null string = []
  | otherwise = auxiliary [] string
  where
    auxiliary _ [] = error "Input may not be empty."
    auxiliary _ [_] = []
    auxiliary taken (x:xs) = (new_taken, xs) : auxiliary new_taken xs
      where
        new_taken = taken ++ [x]

-- | Computes the last set with memoization.
lastMemoized ::
  (Ord nt, Ord t) =>
  AlphaBetaMemoizedContext nt t ->
  [Symbol nt t] ->
  (Set [t], AlphaBetaMemoizedContext nt t)
lastMemoized ctx =
  Bifunctor.first (Set.map reverse)
    . firstMemoized ctx
    . reverse

-- | Computes the first set with memoization.
firstMemoized ::
  (Ord nt, Ord t) =>
  AlphaBetaMemoizedContext nt t ->
  [Symbol nt t] ->
  (Set [t], AlphaBetaMemoizedContext nt t)
firstMemoized ctx [] = (Set.singleton [], ctx)
firstMemoized ctx wi = updatCtx $ runMemoized alphaBetaProducts' wi state
  where
    state = alphaBetaState ctx
    alphaBetaProducts' = alphaBetaFunction ctx
    updatCtx = Bifunctor.second (\state' -> ctx { alphaBetaState = state' })

-- | Given a first map which maps nonterminals to their first sets and a string
-- of symbols compute the first set of that string.
first' :: (Ord nt, Ord t) => Int -> Map nt (Set [t]) -> [Symbol nt t] -> Set [t]
first' _ _ [] = Set.singleton []
first' k first_map wi = alphaBetaProducts k first_map wi

-- | Computes the first map for a given grammar, which maps nonterminals to
-- their first sets.
firstMap :: (Show nt, Show t, Ord nt, Ord t) => Int -> Grammar nt t -> Map nt (Set [t])
firstMap k grammar = fixedPointIterate (/=) f init_first_map
  where
    init_first_map = Map.fromList . map (,Set.empty) $ nonterminals grammar
    f first_map = Map.unionsWith Set.union $ map (auxiliary first_map) (productions grammar)
    auxiliary first_map (Production ai wi) = Map.adjust (Set.union new_set) ai first_map
      where
        new_set = first' k first_map wi

-- | Computes the first set for a given grammar using a string of symbols.
first :: (Show nt, Show t, Ord nt, Ord t) => Int -> Grammar nt t -> [Symbol nt t] -> Set [t]
first k grammar = first' k first_map
  where
    first_map = firstMap k grammar

unextendMap :: (Ord nt, Ord t) =>
  Map (ExtendedNonterminal nt) (Set [ExtendedTerminal t]) ->
  Map nt (Set [t])
unextendMap = unextendValues . unextendKeys
  where
    unextendKeys =
      Map.mapKeys unextendNT
      . Map.filterWithKey (\k _ -> k/=ExtendedStart)
    unextendValues = fmap (Set.map (fmap unextendT . filter (/=End)))

initFollowMap ::
  Ord nt =>
  Int ->
  Grammar (ExtendedNonterminal nt) (ExtendedTerminal t) ->
  ExtendedNonterminal nt ->
  Map (ExtendedNonterminal nt) (Set [ExtendedTerminal t])
initFollowMap k grammar old_start =
  Map.insert old_start stopper
    . Map.fromList
    . map (,Set.empty)
    $ nonterminals grammar
  where
    stopper = Set.singleton $ replicate k End

-- | Computes the first set within the LLP Context such that memoization can be
-- used.
useFirst ::
  (Ord nt, Ord t) =>
  [Symbol nt t] ->
  State (AlphaBetaMemoizedContext nt t) (Set [t])
useFirst syms = do
  ctx <- get
  let (set, ctx') = firstMemoized ctx syms
  put ctx'
  return set

followMap' ::
  (Ord nt, Ord t, Show nt, Show t) =>
  Int ->
  Grammar (ExtendedNonterminal nt) (ExtendedTerminal t) ->
  ExtendedNonterminal nt ->
  State (AlphaBetaMemoizedContext (ExtendedNonterminal nt) (ExtendedTerminal t))
  (Map (ExtendedNonterminal nt) (Set [ExtendedTerminal t]))
followMap' k grammar old_start = fixedPointIteration init_follow_map
  where
    init_follow_map = initFollowMap k grammar old_start
    right_productions = concatMap rightProductons $ productions grammar
    
    fixedPointIteration follow_map = do
      result <- mapM (auxiliary follow_map) right_productions
      let new_follow_map = Map.unionsWith Set.union result
      if follow_map == new_follow_map
        then return new_follow_map
        else fixedPointIteration new_follow_map
    
    auxiliary follow_map' (aj, (ai, w')) = do
      first_set <- useFirst w'
      let subset = truncatedProduct k first_set (follow_map' Map.! aj)
      return $ Map.adjust (Set.union subset) ai follow_map'

-- | Creates the follow map which maps nonterminals to their follow sets.
followMap :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Map nt (Set [t])
followMap k grammar = unextendMap first_map
  where
    first_ctx = initFirstMemoizedContext k extended_grammar
    first_map = evalState (followMap' k extended_grammar old_start) first_ctx
    extended_grammar = extendGrammar k grammar
    old_start = ExtendedNonterminal $ start grammar

lastAndBefore ::
  (Show nt, Show t, Ord nt, Ord t) =>
  Int ->
  Grammar nt t ->
  (AlphaBetaMemoizedContext nt t, nt -> Set [t])
lastAndBefore q grammar = (last_ctx, (before_map Map.!))
  where
    reversed_grammar = reverseGrammar grammar
    before_map = Set.map reverse <$> follow_map
    (last_ctx, follow_map) = firstAndFollow' q reversed_grammar 

firstAndFollow ::
  (Show nt, Show t, Ord nt, Ord t) =>
  Int ->
  Grammar nt t ->
  (AlphaBetaMemoizedContext nt t, nt -> Set [t])
firstAndFollow k grammar = (first_ctx, (follow_map Map.!))
  where
    (first_ctx, follow_map) = firstAndFollow' k grammar

firstAndFollow' ::
  (Show nt, Show t, Ord nt, Ord t) =>
  Int ->
  Grammar nt t ->
  (AlphaBetaMemoizedContext nt t, Map nt (Set [t]))
firstAndFollow' k grammar = (first_ctx, follow_map)
  where
    (first_map, follow_map) = firstAndFollowMaps k grammar
    first_ctx = AlphaBetaMemoizedContext 
      {
        alphaBetaFunction = memoAlphaBetaProducts k first_map,
        alphaBetaState = Map.empty,
        look = k
      }

firstAndFollowMaps ::
  (Show nt, Show t, Ord nt, Ord t) =>
  Int ->
  Grammar nt t ->
  (Map nt (Set [t]), Map nt (Set [t]))
firstAndFollowMaps k grammar = (first_map, follow_map)
  where
    extended_first_map = firstMap k extended_grammar
    old_start = ExtendedNonterminal $ start grammar
    extended_grammar = extendGrammar k grammar
    initFollowMap' = followMap' k extended_grammar old_start
    extended_follow_map = evalState initFollowMap' extended_first_ctx
    follow_map = unextendMap extended_follow_map
    first_map = unextendMap extended_first_map
    extended_first_ctx = AlphaBetaMemoizedContext 
      {
        alphaBetaFunction = memoAlphaBetaProducts k extended_first_map,
        alphaBetaState = Map.empty,
        look = k
      }

-- | Computes the follow set for a given nonterminal.
follow :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> nt -> Set [t]
follow k grammar = (followMap'' Map.!)
  where
    followMap'' = followMap k grammar

-- | Computes the last set for a given string of symbols.
last :: (Show nt, Show a, Ord nt, Ord a) => Int -> Grammar nt a -> [Symbol nt a] -> Set [a]
last q grammar = Set.map reverse . lasts . reverse
  where
    lasts = first q $ reverseGrammar grammar

-- | Computes the before set for a given nonterminal.
before :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> nt -> Set [t]
before q grammar = Set.map reverse . (befores Map.!)
  where
    befores = followMap q $ reverseGrammar grammar

-- | Creates a LL(k) table for a given grammar.
llTable :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Maybe (Map (nt, [t]) Int)
llTable k grammar = do
    let keys = Map.keysSet <$> tables
    let result = unionsIfDisjoint keys
    case result of
      Just _ -> Just $ Map.unions tables
      _ -> Nothing
  where
    first'' = first k grammar
    follow'' = follow k grammar
    prods = productions grammar
    tables = zipWith tableEntry [0 ..] prods
    tableEntry i (Production nt a) = Map.fromList [((nt, y), i) | y <- first_follow_prod]
      where
        first_set = first'' a
        follow_set = follow'' nt
        first_follow_prod = toList $ truncatedProduct k first_set follow_set

-- | Creates a LL(k) table for a given grammar.
llTableM :: (Ord nt, Ord t, Show nt, Show t) =>
  Int ->
  Grammar nt t ->
  (nt -> Set [t]) ->
  State (AlphaBetaMemoizedContext nt t) (Maybe (Map (nt, [t]) Int))
llTableM k grammar follow'' = do
    let prods = productions grammar
    tables <- zipWithM tableEntry [0..] prods
    let table = Map.unionsWith Set.union tables
    let unwrapped = head . Set.toList <$> table
    return $ if any ((>1) . Set.size) table
      then Nothing
      else Just unwrapped
    where
      tableEntry i (Production nt a) = do
        first_set <- useFirst a
        let follow_set = follow'' nt
        let first_follow_prod = toList $ truncatedProduct k first_set follow_set
        return $ Map.fromList [((nt, y), Set.singleton i) | y <- first_follow_prod]

unionIfDisjoint :: Ord a => Set a -> Set a -> Maybe (Set a)
unionIfDisjoint a b = 
  if a `Set.disjoint` b
    then Just $ a `Set.union` b
    else Nothing 

unionsIfDisjoint :: Ord a =>  [Set a] -> Maybe (Set a)
unionsIfDisjoint = foldM unionIfDisjoint Set.empty

-- | Given a 3-tuple where the first element is the input string, the second is
-- the current push down store and the third element is the productions used.
-- This tuple is parsed and a new 3-tuple of the same form is returned after
-- every expansion and popping of terminals is performed. If parsing errors
-- occours nothing is returned.
llParse ::
  (Ord nt, Ord t, Show nt, Show t) =>
  Int ->
  Grammar nt t ->
  ([t], [Symbol nt t], [Int]) ->
  Maybe ([t], [Symbol nt t], [Int])
llParse k grammar = parse
  where
    maybe_table = llTable k grammar
    production_map = Map.fromList . zip [0 ..] $ productions grammar
    parse (x : xs, (Terminal y) : ys, parsed)
      | x == y = parse (xs, ys, parsed)
      | otherwise = Nothing
    parse (input, (Nonterminal y) : ys, parsed) = do
      table <- maybe_table
      index <- (y, take k input) `Map.lookup` table
      production <- index `Map.lookup` production_map
      parse (input, symbols production ++ ys, index : parsed)
    parse ([], [], parsed) = Just ([], [], reverse parsed)
    parse (_, _, _) = Nothing

-- https://zerobone.net/blog/cs/non-productive-cfg-rules/
closureAlgorithm :: (Ord nt, Ord t, Show nt, Show t) => Grammar nt t -> Set nt
closureAlgorithm grammar = fixedPointIterate (/=) (`newProductives` prods) Set.empty
  where
    prods = productions grammar
    isProductive1 set (Nonterminal nt) = nt `Set.member` set
    isProductive1 _ (Terminal _) = True
    isProductive set = all (isProductive1 set) . symbols
    newProductives set = Set.fromList . fmap nonterminal . List.filter (isProductive set)

-- Note: I am not sure if this is one hundred percent correct.
leftFactorNonterminals :: (Ord nt, Ord t) => Grammar nt t -> [nt]
leftFactorNonterminals grammar = Map.keys left_factor_map
  where
    productions_map = toProductionsMap $ productions grammar
    left_factor_map = Map.filter (auxiliary []) productions_map
    commonLeftFactor [] [] = False
    commonLeftFactor [] _ = False
    commonLeftFactor _ [] = False
    commonLeftFactor (a:_) (b:_) = a == b
    auxiliary _ [] = False
    auxiliary ys (x:xs)
      | any (x `commonLeftFactor`) xs || any (x `commonLeftFactor`) ys = True 
      | otherwise = auxiliary (x:ys) xs

data Mark = Unmarked | Temporary | Permanent deriving (Eq, Ord, Show)

-- | Checks if a grammar contains left recursion.
-- It uses dfs topological sorting https://en.wikipedia.org/wiki/Topological_sorting
isLeftRecursive :: (Ord nt, Ord t, Show nt, Show t) => Grammar nt t -> Bool
isLeftRecursive grammar = isNothing $ foldM dfs unmarked_map nonterminals'
  where
    nonterminals' = nonterminals grammar
    nullableOne' = nullableOne grammar
    left_rec_graph = Map.unionsWith Set.union $ makeRecGraph <$> productions grammar

    makeRecGraph (Production nt []) = Map.singleton nt Set.empty
    makeRecGraph (Production nt ((Terminal _):_)) = Map.singleton nt Set.empty
    makeRecGraph (Production nt s@((Nonterminal nt'):_)) = all_nodes
      where
        toMap a b = Map.singleton a $ Set.singleton b
        first_node = toMap nt nt'
        tail' = tail s
        init' = init s
        zipped = takeWhile (uncurry predicate) $ zip init' tail'
        predicate a b = isNonterminal a && isNonterminal b && nullableOne' a
        other_maps = toMap nt . unpackNonterminal . snd <$> zipped
        other_nodes = Map.unionsWith Set.union other_maps
        all_nodes = Map.unionWith Set.union first_node other_nodes

    unmarked_map = Map.fromList $ map (, Unmarked) nonterminals'

    dfs marks node
      | node_mark == Permanent = Just marks
      | node_mark == Temporary = Nothing
      | otherwise = do
        new_marks <- mapM (dfs marks_with_temp) neighbours
        let pre_perm_marks = Map.unionsWith max new_marks `Map.union` marks
        return $ Map.insert node Permanent pre_perm_marks
      where
        node_mark = marks Map.! node
        marks_with_temp = Map.insert node Temporary marks
        neighbours = toList $ left_rec_graph Map.! node
