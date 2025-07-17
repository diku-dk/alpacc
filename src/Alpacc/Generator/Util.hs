module Alpacc.Generator.Util
  ( lpad,
    rpad,
    padLLPTableKeys,
    toIntLLPTable,
    llpHashTable,
    padLLPTableValues,
  )
where

import Alpacc.Encode
import Alpacc.Grammar
import Alpacc.HashTable
import Alpacc.LLP
  ( Bracket (..),
    llpParserTableWithStartsHomomorphisms,
  )
import Alpacc.Types
import Control.DeepSeq
import Data.Bifunctor qualified as BI
import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Tuple.Extra
import Data.Word

-- | Adds m padding to the left side of a list.
lpad :: a -> Int -> [a] -> [a]
lpad p m xs = replicate (m - length ys) p ++ ys
  where
    ys = take m xs

-- | Adds m padding to the right side of a list.
rpad :: a -> Int -> [a] -> [a]
rpad p m xs = ys ++ replicate (m - length ys) p
  where
    ys = take m xs

padLLPTableKeys ::
  (Ord t) =>
  t ->
  Int ->
  Int ->
  Map ([t], [t]) a ->
  Map ([t], [t]) a
padLLPTableKeys t q k =
  Map.mapKeys (BI.bimap frontPad backPad)
  where
    frontPad = lpad t q
    backPad = rpad t k

toIntLLPTable ::
  (Ord nt, Ord t) =>
  SymbolEncoder nt t ->
  Map
    ( [AugmentedTerminal (Unused t)],
      [AugmentedTerminal (Unused t)]
    )
    ( [Bracket (Symbol (AugmentedNonterminal (Symbol nt t)) (AugmentedTerminal (Unused t)))],
      [Int]
    ) ->
  Map ([Integer], [Integer]) ([Bracket Integer], [Int])
toIntLLPTable encoder table = table'
  where
    table_index_keys = Map.mapKeys (both (fmap (toIndex . Terminal))) table
    table' = first (fmap (fmap toIndex)) <$> table_index_keys
    toIndex = fromJust . flip symbolLookup encoder

padLLPTableValues ::
  Int ->
  Int ->
  Map ([t], [t]) ([a], [b]) ->
  Map ([t], [t]) ([Maybe a], [Maybe b])
padLLPTableValues max_ao max_pi =
  fmap (BI.bimap aoPad piPad)
  where
    aoPad = rpad Nothing max_ao . fmap Just
    piPad = rpad Nothing max_pi . fmap Just

flatten :: (Ord k) => Map k [v] -> ([v], Map k (Int, Int))
flatten m = (concat vs, m')
  where
    (ks, vs) = unzip $ Map.toList m
    offsets = scanl (+) 0 $ map length vs
    starts = init offsets
    ends = tail offsets
    vs' = zip starts ends
    m' = Map.fromList $ zip ks vs'

flattenTuple :: (Ord k) => Map k ([v], [v']) -> ([v], [v'], Map k ((Int, Int), (Int, Int)))
flattenTuple m = (v0, v1, m')
  where
    (v0, m0) = flatten $ fst <$> m
    (v1, m1) = flatten $ snd <$> m
    m' =
      Map.fromList $
        zipWith
          (\(k, x) (k', x') -> if k == k' then (k, (x, x')) else error "This shoul not happen.")
          (Map.toList m0)
          (Map.toList m1)

hash :: [Integer] -> Word64
hash =
  foldl'
    ( \h a ->
        let h' = h ^ a
         in h' * 1099511628211
    )
    14695981039346656037
    . map fromIntegral

llpHashTable ::
  (Show nt, Show t, Ord nt, Ord t, NFData nt, NFData t, IntType i, Show i, Ord i) =>
  Int ->
  Int ->
  Integer ->
  ParsingGrammar nt t ->
  SymbolEncoder nt t ->
  Either
    Text
    ( [Bracket Integer],
      [Int],
      OpenAddressing [Integer] ((Int, Int), (Int, Int))
    )
llpHashTable q k empty_terminal grammar encoder = do
  table <- llpParserTableWithStartsHomomorphisms q k $ getGrammar grammar

  let int_table =
        Map.mapKeys (uncurry (<>)) $
          padLLPTableKeys empty_terminal q k $
            toIntLLPTable encoder table
      (stacks, prods, flat_int_table) = flattenTuple int_table
      oa = openAdressing hash flat_int_table
  pure $ (stacks, prods, oa)
