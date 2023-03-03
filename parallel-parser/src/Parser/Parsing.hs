module Parser.Parsing
  ( nullable,
    first,
    follow,
    last,
    before,
    llpItems,
    Item(..),
    moveDots
  )
where
import Data.Foldable (toList)
import Data.Function (flip, ($), (.))
import qualified Data.List as L
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Map ( Map(..) )
import qualified Data.Set as S
import Data.Set ( Set(..) )
import qualified Data.Sequence as SQ
import Data.Sequence ( Seq(..), (><))
import Parser.Grammar
import Prelude hiding (last)
import Data.Maybe
import Data.Composition

toProductionsMap :: (Ord nt, Ord t) => [Production nt t] -> Map nt [[Symbol nt t]]
toProductionsMap = M.fromList . fmap toPair . L.groupBy nonterminalEq . L.sort
  where
    nonterminalEq a b = nonterminal a == nonterminal b
    toPair a = (nonterminal $ head a, symbols <$> a)

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

-- I think i misunderstood the definition.
-- first :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> [Symbol nt t] -> Set [t]
-- first k grammar =
--   removeEpsilon
--     . reverseTerminals
--     . S.unions
--     . takeWhile (not . S.null)
--     . iterate (S.map tail . removeEpsilon)
--     . reverseTerminals
--     . firstAB k grammar first_map
--   where
--     first_map = firsts k grammar
--     removeEpsilon = S.filter (not . L.null)
--     reverseTerminals = S.map L.reverse

first :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar nt t -> [Symbol nt t] -> Set [t]
first k grammar = S.filter (not . L.null) . firstAB k grammar first_map
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

toDotPoduction :: Production nt t -> DotProduction nt t
toDotPoduction (Production nt s) = DotProduction nt s []

data Item nt t = Item
  { production :: DotProduction nt t,
    suffix :: [t],
    prefix :: [t],
    shortestPrefix :: [t]
  } deriving (Show)

initD :: (Show nt, Show t, Ord nt, Ord t) => Int -> Grammar nt t -> Item nt t
initD q grammar =
  Item
    { production = toDotPoduction production',
      suffix = if null last' then [] else S.findMin last',
      prefix = [],
      shortestPrefix = []
    }
  where
    production' = head $ findProductions grammar (start grammar)
    last' = last q grammar $ symbols production'

moveDot (DotProduction nt s s') = DotProduction nt (init s) (L.last s : s')
moveDots = takeWhileNMore 1 isNotEpsilon . iterate moveDot
  where
    isNotEpsilon (DotProduction _ s _) = not $ L.null s

isPrefix :: (Eq a) => Seq a -> Seq a -> Bool
isPrefix = all (uncurry (==)) .: SQ.zip

expansions :: (Eq nt, Show nt, Show t) => Grammar nt t -> Seq (Symbol nt t) -> Seq (Symbol nt t) -> Seq (Seq (Symbol nt t))
expansions _ _ SQ.Empty = SQ.empty
expansions grammar previous ((Terminal x) :<| xs) = expansions grammar (previous :|> Terminal x) xs
expansions grammar previous ((Nonterminal x) :<| xs) = prod >< expansions grammar (previous :|> Nonterminal x) xs
  where
    prod = SQ.fromList $ between . SQ.fromList . symbols <$> L.filter (\(Production nt _) -> nt == x) (productions grammar)
    between p = previous >< p >< xs

solveShortestsPrefix :: (Eq nt, Eq t, Show nt, Show t) => Grammar nt t -> Seq (Symbol nt t) -> Seq (Symbol nt t) -> [t]
solveShortestsPrefix grammar seq prefix = toTerminal <$> (toList . auxiliary $ SQ.fromList [seq])
  where
    toTerminal (Terminal t) = t
    auxiliary SQ.Empty = SQ.empty
    auxiliary (head :<| queue)
      | isPrefix prefix head = head
      | otherwise = auxiliary (queue >< expansions grammar SQ.Empty head)

newD :: (Ord t, Ord nt, Show nt, Show t) => Int -> Int -> Grammar nt t -> [t] -> [Symbol nt t] -> DotProduction nt t -> Item nt t
newD q k grammar vi delta dot_production =
  Item
    { production = dot_production,
      suffix = uj,
      prefix = vj,
      shortestPrefix = gamma
    }
  where
    uj = S.findMin . S.unions $ S.map (last q grammar . (++alpha) . fmap Terminal) (before q grammar y)
    vj = S.findMin . first k grammar $ fmap Terminal vi
    gamma = solveShortestsPrefix grammar x_delta (SQ.fromList alpha)
    x = L.head x_beta
    x_delta = SQ.fromList $ x:delta
    (DotProduction y alpha x_beta) = dot_production

addAnotherD :: (Ord nt, Show nt, Show t, Ord t) => Int -> Grammar nt t -> [Symbol nt t] -> Item nt t -> Item nt t
addAnotherD q grammar delta
    (Item { production = DotProduction x alpha_y beta,
      suffix = _,
      prefix = v,
      shortestPrefix = gamma
    }) = Item { production = DotProduction y delta [],
      suffix = u',
      prefix = v,
      shortestPrefix = gamma
    }
  where
    u' = S.findMin . S.unions $ S.map (last q grammar . (++delta) . fmap Terminal) (before q grammar y)
    y = (\(Nonterminal nt) -> nt) $ L.last alpha_y

llpItems :: (Show nt, Show t, Ord nt, Ord t) => Int -> p -> Grammar nt t -> Item (AugmentedNonterminal nt) (AugmentedTerminal t)
llpItems q k grammar = d_init
  where
    augmented_grammar = augmentGrammar grammar
    d_init = initD q augmented_grammar