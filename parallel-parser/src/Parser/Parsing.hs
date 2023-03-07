module Parser.Parsing
  ( nullable,
    first,
    follow,
    last,
    before,
    Item (..),
    moveDots,
    llpItems,
    DotProduction (..),
    toDotProduction,
    llkParse,
  )
where

import Data.Composition
import Data.Foldable (toList)
import Data.Function (flip, ($), (.))
import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Map (Map (..))
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as SQ
import Data.Set (Set (..))
import qualified Data.Set as S
import Debug.Trace (traceShow)
import Parser.Grammar
import Prelude hiding (last)

debug x = traceShow ("DEBUG: " ++ show x) x

checkpoint = traceShow "Checkpoint"

fixedPointIterate :: Eq b => (b -> b) -> b -> b
fixedPointIterate f a = fst . head . dropWhile (uncurry (/=)) . drop 1 $ iterate swapApply (a, a)
  where
    swapApply (n, _) = (f n, n)

nullables :: (Ord nt, Ord t, Show nt, Show t) => Grammar nt t -> Map nt Bool
nullables grammar = fixedPointIterate (nullableNontermianl productions_map) init_nullable_map
  where
    init_nullable_map = M.fromList . map (,False) $ nonterminals grammar
    nullable' _ (Terminal _) = False
    nullable' nullable_map (Nonterminal a) = nullable_map M.! a
    productions_map = toProductionsMap $ productions grammar
    nullableNontermianl prods nullable_map = (any . all $ nullable' nullable_map) <$> prods

nullableOne :: (Ord nt, Ord t, Show nt, Show t) => Grammar nt t -> Symbol nt t -> Bool
nullableOne _ (Terminal t) = False
nullableOne grammar (Nonterminal nt) = nullable_map M.! nt
  where
    nullable_map = nullables grammar

nullable :: (Ord nt, Ord t, Show nt, Show t) => Grammar nt t -> [Symbol nt t] -> Bool
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
  (Ord nt, Ord t, Show nt, Show t) =>
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

firsts :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Map nt (Set [t])
firsts k grammar = fixedPointIterate (firstNontermianl productions_map) init_first_map
  where
    firstAB' = firstAB k grammar
    init_first_map = M.fromList . map (,S.singleton []) $ nonterminals grammar
    productions_map = toProductionsMap $ productions grammar
    firstNontermianl prods first_map = fmap (S.unions . fmap (firstAB' first_map)) prods

first :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> [Symbol nt t] -> Set [t]
first k grammar = firstAB k grammar first_map
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

constraints :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> [Set (Constraint nt t)]
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

follows :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Map nt (Set [t])
follows k grammar = fixedPointIterate f init_follow_map
  where
    constraints' = S.unions $ constraints k grammar
    init_follow_map = M.fromList . map (,S.empty) $ nonterminals grammar
    addConstraint (TConstraint t nt) m = M.adjust (`S.union` t) nt m
    addConstraint (NTConstraint nt nt') m = M.adjust (`S.union` (m M.! nt)) nt' m
    f a = foldl (flip addConstraint) a constraints'

follow :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> nt -> Set [t]
follow k grammar = (follows' M.!)
  where
    follows' = follows k grammar

last :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> [Symbol nt t] -> Set [t]
last q grammar = S.map reverse . lasts . reverse
  where
    lasts = first q $ reverseGrammar grammar

before :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> nt -> Set [t]
before q grammar = S.map reverse . (befores M.!)
  where
    befores = follows q $ reverseGrammar grammar

data DotProduction nt t = DotProduction nt [Symbol nt t] [Symbol nt t] deriving (Ord, Eq, Show, Read)

toDotProduction :: Production nt t -> DotProduction nt t
toDotProduction (Production nt s) = DotProduction nt s []

data Item nt t = Item
  { dotProduction :: DotProduction nt t,
    suffix :: Set [t],
    prefix :: Set [t],
    shortestPrefix :: [Symbol nt t]
  }
  deriving (Eq, Ord, Show)

initD :: (Show nt, Show t, Ord nt, Ord t) => Int -> Grammar nt t -> Item nt t
initD q grammar =
  Item
    { dotProduction = toDotProduction production',
      suffix = if null last' then S.singleton [] else last',
      prefix = S.singleton [],
      shortestPrefix = []
    }
  where
    production' = head $ findProductions grammar (start grammar)
    last' = last q grammar $ symbols production'

moveDot :: DotProduction nt t -> DotProduction nt t
moveDot (DotProduction nt s s') = DotProduction nt (init s) (L.last s : s')

moveDots :: DotProduction nt t -> [DotProduction nt t]
moveDots = takeWhileNMore 1 isNotEpsilon . iterate moveDot
  where
    isNotEpsilon (DotProduction _ s _) = not $ L.null s

firstTable :: (Show nt, Show t, Ord nt, Ord t) => Int -> Grammar nt t -> Map (nt, [t]) Int
firstTable k grammar = M.unions $ auxiliary <$> zip (productions grammar) [0 ..]
  where
    first' = first k grammar
    auxiliary (Production nt a, i) = M.fromList [((nt, y), i) | y <- S.toList $ first' a]

nullableFollowTable :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Map (nt, [t]) Int
nullableFollowTable k grammar = M.unions $ auxiliary <$> zip [0 ..] (productions grammar)
  where
    follow' = follow k grammar
    nullable' = nullable grammar
    auxiliary (i, Production nt a) = M.fromList [((nt, y), i) | nullable' a, y <- S.toList $ follow' nt]

llkTable :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> Map (nt, [t]) Int
llkTable k grammar = M.union first_table nullable_follow_table
  where
    first_table = firstTable k grammar
    nullable_follow_table = nullableFollowTable k grammar

llkParse :: (Show nt, Show t, Ord nt, Ord t) => Int -> Grammar nt t -> ([t], [Symbol nt t], [Int]) -> Maybe ([t], [Symbol nt t], [Int])
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

expansions :: (Ord t, Show nt, Show t, Ord nt) => Grammar nt t -> [Symbol nt t] -> Seq [Symbol nt t]
expansions grammar s = expand SQ.empty SQ.empty (SQ.fromList s)
  where
    production_map = fmap (fmap SQ.fromList) . toProductionsMap $ productions grammar
    expand es _ SQ.Empty = es
    expand es ys ((Terminal x) :<| xs) = expand es (ys :|> Terminal x) xs
    expand es ys ((Nonterminal x) :<| xs) = expand (es >< ps) (ys :|> Nonterminal x) xs
      where
        smallExpand e = ys >< e >< xs
        ps = mapM smallExpand (production_map M.! x)

isLeastOneTerminal :: (Show nt, Show t, Ord t, Ord nt) => Grammar nt t -> [Symbol nt t] -> Bool
isLeastOneTerminal grammar = bfs S.empty . SQ.singleton
  where
    expansions' = expansions grammar
    bfs _ SQ.Empty = False
    bfs visited (top :<| queue)
      | top `S.member` visited = bfs new_visited queue
      | null top = bfs new_visited queue
      | isTerminal $ head top = True
      | otherwise = bfs new_visited (queue >< expansions' top)
      where
        new_visited = S.insert top visited

solveShortestsPrefix :: (Show nt, Show t, Ord t, Ord nt) => Grammar nt t -> [Symbol nt t] -> [Symbol nt t]
solveShortestsPrefix _ [] = []
solveShortestsPrefix grammar s = solveShortestsPrefix' s
  where
    prefixes = reverse . takeWhile (not . L.null) . iterate init
    solveShortestsPrefix' = head . dropWhile (not . isLeastOneTerminal grammar) . prefixes

newLlpItem :: (Ord t, Ord nt, Show nt, Show t) => Int -> Int -> Grammar nt t -> Set [t] -> [Symbol nt t] -> DotProduction nt t -> Item nt t
newLlpItem q k grammar vi delta dot_production = item
  where
    uj = S.filter (not . null) . S.unions $ (last q grammar . (++ alpha) . fmap Terminal) `S.map` S.insert [] (before q grammar y)
    vj = S.filter (not . null) . S.unions $ (first k grammar . (x ++) . fmap Terminal) `S.map` S.insert [] vi
    x = [L.head x_beta | not (L.null x_beta)]
    x_delta = x ++ delta
    shortest_prefix = solveShortestsPrefix grammar x_delta
    item =
      Item
        { dotProduction = dot_production,
          suffix = uj,
          prefix = vj,
          shortestPrefix = shortest_prefix
        }
    (DotProduction y alpha x_beta) = dot_production

addLlpItem :: (Ord t, Ord nt, Show nt, Show t) => Int -> Grammar nt t -> p -> Item nt t -> Set (Item nt t)
addLlpItem q grammar items old_item
  | null alpha_y = S.empty
  | isTerminal $ L.last alpha_y = S.empty
  | otherwise = S.fromList $ newItem <$> deltas
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
      newItem delta =
        Item
        { dotProduction = DotProduction y delta [],
          suffix = u',
          prefix = v,
          shortestPrefix = gamma
        }
        where
          u' = S.filter (not . null) . S.unions $ (last q grammar . (++ delta) . fmap Terminal) `S.map` before q grammar y

addLlpItems :: (Ord t, Ord nt, Show nt, Show t) => Int -> Grammar nt t -> Set (Item nt t) -> Set (Item nt t)
addLlpItems q grammar items = S.union items . S.unions $ addLlpItem q grammar items `S.map` items

initialLlpItems :: (Ord t, Ord nt, Show nt, Show t) => Int -> Int -> Grammar nt t -> Item nt t -> Set (Item nt t)
initialLlpItems q k grammar llp_item = S.fromList $ newLlpItem' <$> moveDots dot_production
  where
    newLlpItem' = newLlpItem q k grammar v delta
    dot_production = dotProduction llp_item
    v = prefix llp_item
    delta = shortestPrefix llp_item

solveLlpItem :: (Ord t, Ord nt, Show nt, Show t) => Int -> Int -> Grammar nt t -> Item nt t -> Set (Item nt t)
solveLlpItem q k grammar = fixedPointIterate (addLlpItems q grammar) . initialLlpItems q k grammar

solveLlpItems :: (Ord t, Ord nt, Show nt, Show t) => Int -> Int -> Grammar nt t -> Set (Item nt t) -> Set (Set (Item nt t))
solveLlpItems q k grammar = S.map (solveLlpItem q k grammar)

findProperSubsets items = S.filter (\item -> not $ any (S.isProperSubsetOf item) items) items

llpItems q k grammar = fixedPointIterate unionSolveLlpItems d_init
  where
    augmented_grammar = augmentGrammar grammar
    d_init = S.singleton $ initD q augmented_grammar
    solveLlpItems' = solveLlpItems q k augmented_grammar 
    unionSolveLlpItems = S.unions . solveLlpItems'