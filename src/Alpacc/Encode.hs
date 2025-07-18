module Alpacc.Encode
  ( SymbolEncoder,
    TerminalEncoder,
    fromSymbolToTerminalEncoder,
    encodeSymbols,
    encodeTerminals,
    terminalLookup,
    symbolLookup,
    terminalMax,
    symbolMax,
    bracketIntType,
    productionIntType,
    numTerminals,
    numSymbols,
    symbolTerminalIntType,
    terminalIntType,
    terminalDead,
    symbolDead,
    symbolStartTerminal,
    symbolEndTerminal,
    llpHashTable,
    LLPTable (..),
    printTerminals,
  )
where

import Alpacc.Grammar
import Alpacc.HashTable
import Alpacc.LLP
  ( Bracket (..),
    llpParserTableWithStartsHomomorphisms,
  )
import Alpacc.Types
import Control.DeepSeq
import Data.Bifunctor qualified as BI
import Data.Bits
import Data.Either.Extra
import Data.List (foldl', sortOn)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tuple.Extra
import Data.Word

newtype SymbolEncoder nt t
  = SymbolEncoder
  { symbolEncoder ::
      Map
        ( Symbol
            (AugmentedNonterminal (Symbol nt t))
            (AugmentedTerminal (Unused t))
        )
        Integer
  }

newtype TerminalEncoder t
  = TerminalEncoder
  { terminalEncoder :: Map (Unused t) Integer
  }

printTerminals :: (Show t) => TerminalEncoder t -> [Text]
printTerminals =
  ("Terminal Encoding: " :)
    . map snd
    . sortOn fst
    . mapMaybe auxiliary
    . Map.toList
    . terminalEncoder
  where
    auxiliary (Used t, i) = Just (i, Text.pack (show t) <> ": " <> Text.pack (show i))
    auxiliary (Unused, _) = Nothing

fromSymbolToTerminalEncoder :: (Ord t) => SymbolEncoder nt t -> TerminalEncoder t
fromSymbolToTerminalEncoder =
  TerminalEncoder
    . Map.mapKeys (fromMaybe (error "This will never happen."))
    . Map.filterWithKey (\k _ -> isJust k)
    . Map.mapKeys toTerminal
    . symbolEncoder
  where
    toTerminal (Terminal (AugmentedTerminal t)) = Just t
    toTerminal _ = Nothing

findSingleton :: (Eq t) => t -> [t] -> [t]
findSingleton t = maybeToList . List.find (t ==)

encodeSymbols :: (Ord nt, Ord t) => t -> ParsingGrammar nt t -> SymbolEncoder nt t
encodeSymbols t_ignore grammar =
  SymbolEncoder $
    Map.fromList $
      zip s [0 ..]
  where
    ts' = terminals $ getGrammar grammar
    nts = nonterminals $ getGrammar grammar

    (tts, fts) = List.partition p ts'

    ts = fts ++ ignore ++ unused ++ right ++ left

    s = (Terminal <$> ts) ++ (Nonterminal <$> nts)

    left = findSingleton LeftTurnstile tts
    right = findSingleton RightTurnstile tts
    unused = findSingleton (AugmentedTerminal Unused) tts
    ignore = findSingleton (AugmentedTerminal (Used t_ignore)) tts

    p t =
      t == LeftTurnstile
        || t == RightTurnstile
        || AugmentedTerminal Unused == t
        || AugmentedTerminal (Used t_ignore) == t

encodeTerminals :: (Ord t) => t -> ParsingTerminals t -> TerminalEncoder t
encodeTerminals t_ignore ters =
  TerminalEncoder $
    Map.fromList $
      zip s [0 ..]
  where
    ts' = getTerminals ters
    (tts, fts) = List.partition p ts'
    unused = findSingleton Unused tts
    ignore = findSingleton (Used t_ignore) tts
    s = fts ++ ignore ++ unused

    p t =
      Unused == t
        || Used t_ignore == t

terminalLookup :: (Ord t) => t -> TerminalEncoder t -> Maybe Integer
terminalLookup t encoder =
  Map.lookup (Used t) (terminalEncoder encoder)

terminalDead :: (Ord t) => TerminalEncoder t -> Integer
terminalDead encoder =
  fromMaybe (error "This will not happen.") $
    Map.lookup Unused (terminalEncoder encoder)

symbolDead :: (Ord nt, Ord t) => SymbolEncoder nt t -> Integer
symbolDead encoder =
  fromMaybe (error "This will not happen.") $
    Map.lookup dead (symbolEncoder encoder)
  where
    dead = Terminal $ AugmentedTerminal Unused

symbolEndTerminal :: (Ord nt, Ord t) => SymbolEncoder nt t -> Integer
symbolEndTerminal encoder =
  fromMaybe (error "This will not happen.") $
    Map.lookup end (symbolEncoder encoder)
  where
    end = Terminal LeftTurnstile

symbolStartTerminal :: (Ord nt, Ord t) => SymbolEncoder nt t -> Integer
symbolStartTerminal encoder =
  fromMaybe (error "This will not happen.") $
    Map.lookup start (symbolEncoder encoder)
  where
    start = Terminal RightTurnstile

symbolLookup ::
  (Ord t, Ord nt) =>
  Symbol
    (AugmentedNonterminal (Symbol nt t))
    (AugmentedTerminal (Unused t)) ->
  SymbolEncoder nt t ->
  Maybe Integer
symbolLookup s encoder =
  Map.lookup s (symbolEncoder encoder)

symbolMax :: SymbolEncoder nt t -> Integer
symbolMax = maximum . symbolEncoder

terminalMax :: TerminalEncoder t -> Integer
terminalMax = maximum . terminalEncoder

numTerminals :: TerminalEncoder t -> Int
numTerminals = Map.size . terminalEncoder

numSymbols :: TerminalEncoder t -> Int
numSymbols = Map.size . terminalEncoder

bracketIntType ::
  SymbolEncoder nt t ->
  Either Text UInt
bracketIntType =
  maybeToEither err
    . toIntType
    . (2 *)
    . symbolMax
  where
    err = "Error: There are too many terminals and none terminals to find a integral type for the bracket."

productionIntType ::
  ParsingGrammar nt t ->
  Either Text UInt
productionIntType =
  maybeToEither err
    . toIntType
    . fromIntegral
    . length
    . productions
    . getGrammar
  where
    err = "Error: There are too many productions to find a integral type."

symbolTerminalIntType :: SymbolEncoder nt t -> Either Text UInt
symbolTerminalIntType =
  maybeToEither err
    . toIntType
    . maximum
    . Map.filterWithKey (\k _ -> isTerminal k)
    . symbolEncoder
  where
    err = "Error: There are too many symbols to find a integral type."

terminalIntType :: TerminalEncoder t -> Either Text UInt
terminalIntType =
  maybeToEither err
    . toIntType
    . maximum
    . terminalEncoder
  where
    err = "Error: There are too many terminals to find a integral type."

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
        let h' = h `xor` a
         in h' * 1099511628211
    )
    14695981039346656037
    . map fromIntegral

data LLPTable
  = LLPTable
  { llpStacks :: [Bracket Integer],
    llpProductions :: [Int],
    llpOATable :: OpenAddressing [Integer] ((Int, Int), (Int, Int))
  }
  deriving (Show, Ord, Eq)

llpHashTable ::
  (Show nt, Show t, Ord nt, Ord t, NFData nt, NFData t) =>
  Int ->
  Int ->
  Integer ->
  ParsingGrammar nt t ->
  SymbolEncoder nt t ->
  Either Text LLPTable
llpHashTable q k empty_terminal grammar encoder = do
  table <- llpParserTableWithStartsHomomorphisms q k $ getGrammar grammar

  let int_table =
        Map.mapKeys (uncurry (<>)) $
          padLLPTableKeys empty_terminal q k $
            toIntLLPTable encoder table
      (stacks, prods, flat_int_table) = flattenTuple int_table
      oa = openAdressing hash flat_int_table
  pure $ LLPTable stacks prods oa
