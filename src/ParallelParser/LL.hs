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
    alphaBeta,
    firstMemoized,
    lastMemoized,
    initFirstMemoizedContext,
    initLastMemoizedContext,
    AlphaBetaMemoizedContext (..)
  )
where

import Control.Monad.State
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
import qualified Data.Bifunctor as Bifunctor
import Data.Composition
import Debug.Trace (traceShow)

debug x = traceShow x x

-- | Given a production it produces a list of tuples where the first element is
-- the left hand side of the production. The second element is a tuple where the
-- first element is a nonterminal found in the production and the second element
-- is symbols following that nonterminal. 
rightProductons :: Production nt t -> [(nt, (nt, [Symbol nt t]))]
rightProductons (Production aj symbols') = (aj,) <$> rightSymbols symbols'

-- | Given a string of symbols create all the leftmost derivations.
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

-- | Naïvely creates the follow sets for a given string of symbols. This is done
-- using breadth first search and applying first and to the symbols after the
-- nonterminals of a given derivation in the bfs.
naiveFollows :: (Show nt, Show t, Ord nt, Ord t) => Int -> Grammar nt t -> Map nt (Set [t])
naiveFollows k grammar =
  Map.unionWith Set.union init_maps
    . Map.unionsWith Set.union
    $ bfs Set.empty start'
  where
    [Production nt s] = findProductions grammar (start grammar)
    init_maps = Map.fromList $ (,Set.empty) <$> nonterminals grammar
    first' = first k grammar
    start' = Seq.singleton [Nonterminal nt]
    unpackT (Terminal t) = t
    derivations' = derivations grammar
    bfs _ Empty = []
    bfs visited (top :<| queue)
      | right_symbols_set `Set.isSubsetOf` visited = bfs visited queue
      | null right_symbols = bfs new_visited queue
      | otherwise = maps ++ bfs new_visited (queue >< derivations' top)
      where
        right_symbols = Bifunctor.second first' <$> rightSymbols top
        maps = uncurry Map.singleton <$> right_symbols
        right_symbols_set = Set.fromList right_symbols
        new_visited = right_symbols_set `Set.union` visited

-- | The naïve follow set is created and then used as a function.
naiveFollow :: (Show nt, Show t, Ord nt, Ord t) => Int -> Grammar nt t -> nt -> Set [t]
naiveFollow k grammar = (follow_map Map.!)
  where
    follow_map = naiveFollows k grammar

-- | Performs fixed point iteration until a predicate holds true.
fixedPointIterate :: Eq b => (b -> b -> Bool) -> (b -> b) -> b -> b
fixedPointIterate cmp f = fst . head . dropWhile (uncurry cmp) . iterateFunction
  where
    iterateFunction = drop 1 . iterate swapApply . dupe
    swapApply (n, _) = (f n, n)

-- | Determines the nullablity of each nonterminal using the algorithm described
-- in Introduction to Compiler Design.
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

-- | Determines the nullablity of each symbol using the algorithm described
-- in Introduction to Compiler Design.
nullableOne :: (Ord nt, Ord t) => Grammar nt t -> Symbol nt t -> Bool
nullableOne _ (Terminal t) = False
nullableOne grammar (Nonterminal nt) = nullable_map Map.! nt
  where
    nullable_map = nullables grammar

-- | Determines the nullablity of a string of symbols using the algorithm
-- described in Introduction to Compiler Design.
nullable :: (Ord nt, Ord t) => Grammar nt t -> [Symbol nt t] -> Bool
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
alphaBetaProducts k first_map [Terminal t] = Set.singleton [t]
alphaBetaProducts k first_map [Nonterminal nt] = first_map Map.! nt
alphaBetaProducts k first_map string = Set.unions $ map subProducts alpha_betas
  where
    alpha_betas = alphaBeta string
    subProduct = alphaBetaProducts k first_map
    subProducts = uncurry (truncatedProduct k) . both subProduct

-- | Creates a memoized alphaBeta function given a the lookahead and grammar. 
mkMemoAlphaBetaProducts :: (Ord nt, Ord t) => Int -> Grammar nt t -> MemoFunction [Symbol nt t] (Set [t])
mkMemoAlphaBetaProducts k grammar = memoAlphaBetaProducts k first_map
  where
    first_map = firstMap k grammar

-- | A record used to easily contain a memoized alphaBeta function and the
-- current state it is in.
data AlphaBetaMemoizedContext nt t = AlphaBetaMemoizedContext
  { alphaBetaFunction :: MemoFunction [Symbol nt t] (Set [t]),
    alphaBetaState :: Map [Symbol nt t] (Set [t])
  }

-- | Creates the initial context used for the memoized first function.
initFirstMemoizedContext :: (Ord nt, Ord t) => Int -> Grammar nt t -> AlphaBetaMemoizedContext nt t
initFirstMemoizedContext k grammar =
  AlphaBetaMemoizedContext {
    alphaBetaFunction = mkMemoAlphaBetaProducts k grammar,
    alphaBetaState = Map.empty
  }

-- | Creates the initial context used for the memoized last function.
initLastMemoizedContext ::
  (Ord nt, Ord t) =>
  Int ->
  Grammar nt t ->
  AlphaBetaMemoizedContext nt t
initLastMemoizedContext q grammar =
  AlphaBetaMemoizedContext {
    alphaBetaFunction = mkMemoAlphaBetaProducts q (reverseGrammar grammar),
    alphaBetaState = Map.empty
  }

-- | Every possible way to split a string in two.
alphaBeta :: [a] -> [([a], [a])]
alphaBeta string
  | null string = []
  | otherwise = auxiliary [] string
  where
    auxiliary taken [x] = []
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
firstMemoized ctx wi = updatCtx $ runMemoized alphaBetaProducts wi state
      where
        state = alphaBetaState ctx
        alphaBetaProducts = alphaBetaFunction ctx
        updatCtx = Bifunctor.second (\state' -> ctx { alphaBetaState = state' })

-- | Given a first map which maps nonterminals to their first sets and a string
-- of symbols compute the first set of that string.
first' :: (Ord nt, Ord t) => Int -> Map nt (Set [t]) -> [Symbol nt t] -> Set [t]
first' _ _ [] = Set.singleton []
first' k first_map wi = alphaBetaProducts k first_map wi

-- | Computes the first map for a given grammar, which maps nonterminals to
-- their first sets.
firstMap :: (Ord nt, Ord t) => Int -> Grammar nt t -> Map nt (Set [t])
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

-- | Given a string of symbols, find all the nonterminals and make tuples where
-- each nonterminal is the first element of the tuple and the second element is
-- the symbols which comes after that nonterminal.
rightSymbols :: [Symbol nt t] -> [(nt, [Symbol nt t])]
rightSymbols [] = []
rightSymbols ((Terminal _) : xs) = rightSymbols xs
rightSymbols ((Nonterminal x) : xs) = (x, xs) : rightSymbols xs

-- | Creates the follow map which maps nonterminals to their follow sets.
followMap :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Map nt (Set [t])
followMap k grammar = unextendMap $ fixedPointIterate (/=) f init_follow_map
  where
    unextendKeys =
      Map.mapKeys unextendNT
      . Map.filterWithKey (\k _ -> k/=start extended_grammar)
    unextendValues = fmap (Set.map (fmap unextendT . filter (/=End)))
    unextendMap = unextendValues . unextendKeys
    extended_grammar = extendGrammar k grammar
    old_start = ExtendedNonterminal $ start grammar
    first' = first k extended_grammar
    stopper = Set.singleton $ replicate k End
    init_follow_map =
      Map.insert old_start stopper
        . Map.fromList
        . map (,Set.empty)
        $ nonterminals extended_grammar
    f follow_map = Map.unionsWith Set.union $ map (auxiliary follow_map) right_productions
    right_productions = concatMap rightProductons $ productions extended_grammar
    auxiliary follow_map' (aj, (ai, w')) = Map.adjust (Set.union subset) ai follow_map'
      where
        first_set = first' w'
        subset = truncatedProduct k first_set (follow_map' Map.! aj)

-- | Computes the follow set for a given nonterminal.
follow :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> nt -> Set [t]
follow k grammar = (followMap' Map.!)
  where
    followMap' = followMap k grammar

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
llTable :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Map (nt, [t]) Int
llTable k grammar = Map.union first_table follow_table
  where
    first_table = Map.unions $ zipWith firstEntry [0 ..] prods
    follow_table = Map.unions $ zipWith followEntry [0 ..] prods
    prods = productions grammar
    first' = first k grammar
    firstEntry i (Production nt a) = Map.fromList [((nt, y), i) | y <- ts]
      where
        ts = Set.toList . Set.filter (not . null) $ first' a
    follow' = follow k grammar
    followEntry i (Production nt a) =
      Map.fromList [((nt, y), i) | is_nullable, y <- nts]
      where
        nts = Set.toList . Set.filter (not . null) $ follow' nt
        is_nullable = [] `Set.member` first' a

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

        safeHead [] = Nothing
        safeHead (x:_) = Just x

        maybeTuple = do
          key <- safeHead keys
          index <- key `Map.lookup` table
          production <- index `Map.lookup` production_map
          return (index, symbols production)

        Just (index, production) = maybeTuple
