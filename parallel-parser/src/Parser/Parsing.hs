module Parser.Parsing
  ( nullable,
    nullables,
    firsts,
    firstsMaxK,
    toProductionsMap,
    takeWhileNMore,
    fixedPointIterate,
    rightSymbols,
    constraints,
    follows,
    Parser.Parsing.last,
    before
  )
where

import Prelude hiding (last)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Parser.Grammar

toProductionsMap :: [Production] -> M.Map Nonterminal [[Symbol]]
toProductionsMap = M.fromList . fmap toPair . L.groupBy nonterminalEq . L.sort
  where
    nonterminalEq a b = nonterminal a == nonterminal b
    toPair a = (nonterminal $ head a, symbols <$> a)

fixedPointIterate :: Eq b => (b -> b) -> b -> b
fixedPointIterate f a = fst . head . dropWhile (uncurry (/=)) . drop 1 $ iterate swapApply (a, a)
  where
    swapApply (n, _) = (f n, n)

nullables :: Grammar -> [Bool]
nullables grammar = all (nullable' final_nullable_map) . symbols <$> productions grammar
  where
    init_nullable_map = M.fromList . map (,False) $ nonterminals grammar
    nullable' _ (T _) = False
    nullable' nullable_map (NT a) = nullable_map M.! a
    productions_map = toProductionsMap $ productions grammar
    nullableNontermianl prods nullable_map = (any . all $ nullable' nullable_map) <$> prods
    final_nullable_map = fixedPointIterate (nullableNontermianl productions_map) init_nullable_map

nullable :: Grammar -> [Symbol] -> Bool
nullable grammar = all nullable'
  where
    nullable' (NT nt) = nullables' M.! nt
    nullable' (T t) = False
    nonterminals' = nonterminal <$> productions grammar
    nullables' = M.unionsWith (||) . zipWith M.singleton nonterminals' $ nullables grammar

takeWhileNMore :: Int -> (a -> Bool) -> [a] -> [a]
takeWhileNMore _ _ [] = []
takeWhileNMore n predicate (x : xs)
  | predicate x = x : takeWhileNMore n predicate xs
  | otherwise = x : take (n - 1) xs

helper :: Int -> M.Map Nonterminal (S.Set [Terminal]) -> S.Set [Terminal] -> Symbol -> S.Set [Terminal]
helper k first_map first' (T t) = S.map (take k . (++[t])) first'
helper k first_map first' (NT nt) = exteneded_first
  where nt_first = S.toList $ first_map M.! nt
        first_list = S.toList first'
        exteneded_first = S.fromList [take k $ ts ++ ts' | ts <- first_list, ts' <- nt_first ]

firstAB :: Int -> Grammar -> M.Map Nonterminal (S.Set [Terminal]) -> [Symbol] -> S.Set [Terminal]
firstAB k grammar first_map = foldl (helper k first_map) (S.singleton []) . takeWhileNMore k isNullable
  where isNullable = nullable grammar . L.singleton

firstsMaxK :: Int -> Grammar -> [S.Set [Terminal]]
firstsMaxK k grammar = firstAB' final_first_map . symbols <$> productions grammar
  where
    firstAB' = firstAB k grammar
    init_first_map = M.fromList . map (,S.singleton []) $ nonterminals grammar
    productions_map = toProductionsMap $ productions grammar
    firstNontermianl prods first_map = fmap (S.unions . fmap (firstAB' first_map)) prods
    final_first_map = fixedPointIterate (firstNontermianl productions_map) init_first_map

firsts :: Int -> Grammar -> [S.Set [Terminal]]
firsts k grammar = auxiliary <$> firstsMaxK k grammar
  where auxiliary :: S.Set [Terminal] -> S.Set [Terminal]
        auxiliary = S.unions . zipWith (S.map . take) [1..k] . repeat

first :: Int -> Grammar -> [Symbol] -> S.Set [Terminal]
first k grammar = S.filter (not . null) . firstAB k grammar first_map
  where nonterminals' = nonterminal <$> productions grammar
        first_map = M.unionsWith S.union . zipWith M.singleton nonterminals' $ firsts k grammar

rightSymbols :: [Symbol] -> [(Nonterminal, [Symbol])]
rightSymbols [] = []
rightSymbols ((T _) : xs) = rightSymbols xs
rightSymbols ((NT x) : xs) = (x, xs) : rightSymbols xs

data Constraint
  = TConstraint (S.Set [Terminal]) Nonterminal
  | NTConstraint Nonterminal Nonterminal
  deriving (Eq, Ord)

instance Show Constraint where
  show (TConstraint ts nt) = "{" ++ L.intercalate ", " (show <$> S.toList ts) ++ "} ⊆ " ++ show nt
  show (NTConstraint nt nt') = show nt ++ " ⊆ " ++ show nt'

constraints :: Int -> Grammar -> [S.Set Constraint]
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
            ntConstraint =  S.fromList [NTConstraint nt nt' | nt /= nt' && nullable' right]

follows :: Int -> Grammar -> M.Map Nonterminal (S.Set [Terminal])
follows k grammar = fixedPointIterate f init_follow_map
  where
    constraints' = S.unions $ constraints k grammar
    init_follow_map = M.fromList . map (,S.empty) $ nonterminals grammar
    addConstraint (TConstraint t nt) m = M.adjust (`S.union` t) nt m
    addConstraint (NTConstraint nt nt') m = M.adjust (`S.union` (m M.! nt)) nt' m
    f a = foldl (flip addConstraint) a constraints'

follow :: Int -> Grammar -> Nonterminal -> S.Set [Terminal]
follow k grammar = (follows' M.!)
  where
    follows' = follows k grammar

last :: Int -> Grammar -> [Symbol] -> S.Set [Terminal]
last q grammar = S.map reverse . lasts . reverse
  where
    lasts = first q $ reverseGrammar grammar

before :: Int -> Grammar -> Nonterminal -> S.Set [Terminal]
before q grammar =  S.map reverse . (befores M.!)
  where
    befores = follows q $ reverseGrammar grammar