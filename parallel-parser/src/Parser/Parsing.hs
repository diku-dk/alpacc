module Parser.Parsing
  ( nullable,
  )
where

import Parser.Grammar
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Data.Bifunctor (Bifunctor(bimap))

toProductionsMap :: [Production] -> M.Map Nonterminal [[Symbol]]
toProductionsMap = M.fromList . fmap toPair . L.groupBy nonterminalEq . L.sort
  where
    nonterminalEq a b = nonterminal a == nonterminal b
    toPair a = (nonterminal $ head a, symbols <$> a)

fixedPointIterate :: Eq b => (b -> b) -> (b, b) -> b
fixedPointIterate f (a, b) = fst . head . dropWhile (uncurry (/=)) $ iterate helper (a, b)
  where helper (n, _) = (f n, n)

nullable :: Grammar -> [Bool]
nullable grammar = all (nullable' final_nullable_map) . symbols <$> productions grammar
  where
    init_nullable_map = M.fromList . map (,False) $ nonterminals grammar
    nullable' _ (T _) = False
    nullable' nullable_map (NT a) = nullable_map M.! a
    productions_map = toProductionsMap $ productions grammar
    nullableNontermianl nullable_map = fmap . any . all $ nullable' nullable_map
    final_nullable_map = fixedPointIterate (`nullableNontermianl` productions_map) (init_nullable_map, M.empty)
