module Parser.Parsing
  ( nullable,
    nullables,
    firsts,
    first,
    constraints,
    follows,
    follow,
  )
where

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
fixedPointIterate f a = fst . head . dropWhile (uncurry (/=)) . drop 1 $ iterate helper (a, a)
  where
    helper (n, _) = (f n, n)

nullables :: Grammar -> [Bool]
nullables grammar = all (nullable' final_nullable_map) . symbols <$> productions grammar
  where
    init_nullable_map = M.fromList . map (,False) $ nonterminals grammar
    nullable' _ (T _) = False
    nullable' nullable_map (NT a) = nullable_map M.! a
    productions_map = toProductionsMap $ productions grammar
    nullableNontermianl prods nullable_map = (any . all $ nullable' nullable_map) <$> prods
    final_nullable_map = fixedPointIterate (nullableNontermianl productions_map) init_nullable_map

nullableOne :: Grammar -> Symbol -> Bool
nullableOne _ (T _) = False
nullableOne grammar (NT nt) = nullables' M.! nt
  where
    nonterminals' = nonterminal <$> productions grammar
    nullables' = M.unionsWith (||) . zipWith M.singleton nonterminals' $ nullables grammar

nullable :: Grammar -> [Symbol] -> Bool
nullable grammar = all nullable'
  where
    nullable' = nullableOne grammar

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore _ [] = []
takeWhileOneMore predicate (x : xs)
  | predicate x = x : takeWhileOneMore predicate xs
  | otherwise = [x]

firsts :: Grammar -> [S.Set Terminal]
firsts grammar = first_ab final_first_map . symbols <$> productions grammar
  where
    init_first_map = M.fromList . map (,S.empty) $ nonterminals grammar
    first' _ (T t) = S.singleton t
    first' first_map (NT a) = first_map M.! a
    productions_map = toProductionsMap $ productions grammar
    isNullable = nullableOne grammar
    first_ab first_map = S.unions . fmap (first' first_map) . takeWhileOneMore isNullable
    firstNontermianl prods first_map = fmap (S.unions . fmap (first_ab first_map)) prods
    final_first_map = fixedPointIterate (firstNontermianl productions_map) init_first_map

firstOne :: Grammar -> Symbol -> S.Set Terminal
firstOne grammar (T t) =
  if t `elem` terminals grammar
    then S.singleton t
    else error $ show t ++ " is not a valid terminal."
firstOne grammar (NT nt) = firsts' M.! nt
  where
    nonterminals' = nonterminal <$> productions grammar
    firsts' = M.unionsWith S.union . zipWith M.singleton nonterminals' $ firsts grammar

first :: Grammar -> [Symbol] -> S.Set Terminal
first grammar = S.unions . map first'
  where
    first' = firstOne grammar

rightSymbols :: [Symbol] -> [(Nonterminal, [Symbol])]
rightSymbols [] = []
rightSymbols ((T _) : xs) = rightSymbols xs
rightSymbols ((NT x) : xs) = (x, xs) : rightSymbols xs

data Constraint
  = TConstraint (S.Set Terminal) Nonterminal
  | NTConstraint Nonterminal Nonterminal
  deriving (Eq, Ord)

instance Show Constraint where
  show (TConstraint ts nt) = "{" ++ L.intercalate ", " (show <$> S.toList ts) ++ "} ⊆ " ++ show nt
  show (NTConstraint nt nt') = show nt ++ " ⊆ " ++ show nt'

constraints :: Grammar -> [S.Set Constraint]
constraints grammar = helper <$> productions grammar
  where
    nullable' = nullable grammar
    first' = first grammar
    helper (Production nt s) = S.unions $ uncurry auxiliary <$> right_symbols
      where
        right_symbols = rightSymbols s
        auxiliary nt' right = tConstraint `S.union` ntConstraint
          where
            first_set = first' right
            tConstraint = S.fromList [TConstraint first_set nt' | not (S.null first_set)]
            ntConstraint =  S.fromList [NTConstraint nt nt' | nt /= nt' && nullable' right]

follows :: Grammar -> M.Map Nonterminal (S.Set Terminal)
follows grammar = fixedPointIterate f init_follow_map
  where constraints' = S.unions $ constraints grammar 
        init_follow_map = M.fromList . map (,S.empty) $ nonterminals grammar
        addConstraint (TConstraint t nt) m = M.adjust (`S.union` t) nt m
        addConstraint (NTConstraint nt nt') m = M.adjust (`S.union` (m M.! nt)) nt' m
        f a = foldl (flip addConstraint) a constraints'

follow :: Grammar -> Nonterminal -> S.Set Terminal
follow grammar = (follows' M.!)
  where follows' = follows grammar