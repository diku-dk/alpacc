module Parser.ParsingOneSymbol
  (
  )
where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Parser.Grammar
  ( Grammar (nonterminals, productions, terminals),
    Nonterminal,
    Production (Production),
    Symbol (..),
    Terminal,
    nonterminal,
    symbols,
    reverseGrammar
  )
import Parser.Parsing (fixedPointIterate, nullable, rightSymbols, takeWhileNMore, toProductionsMap)

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore = takeWhileNMore 1

firsts :: Grammar -> [S.Set Terminal]
firsts grammar = first_ab final_first_map . symbols <$> productions grammar
  where
    init_first_map = M.fromList . map (,S.empty) $ nonterminals grammar
    first' _ (T t) = S.singleton t
    first' first_map (NT a) = first_map M.! a
    productions_map = toProductionsMap $ productions grammar
    isNullable = nullable grammar . L.singleton
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
first grammar = S.unions . fmap first' . takeWhileOneMore isNullable
  where
    isNullable = nullable grammar . L.singleton
    first' = firstOne grammar

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
            ntConstraint = S.fromList [NTConstraint nt nt' | nt /= nt' && nullable' right]

follows :: Grammar -> M.Map Nonterminal (S.Set Terminal)
follows grammar = fixedPointIterate f init_follow_map
  where
    constraints' = S.unions $ constraints grammar
    init_follow_map = M.fromList . map (,S.empty) $ nonterminals grammar
    addConstraint (TConstraint t nt) m = M.adjust (`S.union` t) nt m
    addConstraint (NTConstraint nt nt') m = M.adjust (`S.union` (m M.! nt)) nt' m
    f a = foldl (flip addConstraint) a constraints'

follow :: Grammar -> Nonterminal -> S.Set Terminal
follow grammar = (follows' M.!)
  where
    follows' = follows grammar

last' :: Grammar -> [Symbol] -> S.Set Terminal
last' grammar = lasts . reverse
  where
    lasts = first $ reverseGrammar grammar

before :: Grammar -> Nonterminal -> S.Set Terminal
before grammar = (befores M.!)
  where
    befores = follows $ reverseGrammar grammar
