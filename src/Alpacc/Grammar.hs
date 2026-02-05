module Alpacc.Grammar
  ( Grammar (..),
    Symbol (..),
    Production (..),
    AugmentedNonterminal (..),
    AugmentedTerminal (..),
    T (..),
    NT (..),
    Unused (..),
    ParsingGrammar,
    ParsingTerminals,
    Pretty (..),
    getTerminals,
    parsingTerminals,
    augmentGrammar,
    symbols,
    nonterminal,
    reverseGrammar,
    findProductions,
    isTerminal,
    isNonterminal,
    toProductionsMap,
    unpackNTTGrammar,
    unpackNonterminal,
    unpackTerminal,
    rightSymbols,
    grammarDuplicates,
    grammarError,
    parsingGrammar,
    getGrammar,
    getNames,
    genNonterminals,
    genGrammar,
  )
where

import Alpacc.Util
import Control.DeepSeq
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Char
import Data.IntMap (IntMap)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text hiding (Text)
import GHC.Generics
import Test.QuickCheck
  ( Gen,
    choose,
    sized,
    vectorOf,
  )

class Pretty a where
  pretty :: a -> Text

-- | Used in augmenting terminals for the augmented grammar.
data AugmentedTerminal t
  = AugmentedTerminal t
  | RightTurnstile
  | LeftTurnstile
  deriving (Ord, Eq, Generic, Show)

instance (NFData t) => NFData (AugmentedTerminal t)

instance (Pretty t) => Pretty (AugmentedTerminal t) where
  pretty (AugmentedTerminal t) = pretty t
  pretty RightTurnstile = "⊢"
  pretty LeftTurnstile = "⊣"

-- | Used in augmenting nonterminals for the augmented grammar.
data AugmentedNonterminal nt
  = AugmentedNonterminal nt
  | Start
  deriving (Ord, Eq, Generic, Show)

instance (NFData nt) => NFData (AugmentedNonterminal nt)

instance (Pretty nt) => Pretty (AugmentedNonterminal nt) where
  pretty (AugmentedNonterminal nt) = pretty nt
  pretty Start = "⊤"

-- | Structure used for terminals making it easier to print strings in a
-- readable manner.
data T = T Text | TLit Text deriving (Ord, Eq, Generic, Show)

instance Pretty T where
  pretty (T t) = t
  pretty (TLit lit) = "\"" <> lit <> "\""

instance NFData T

-- | Structure used for nonterminals making it easier to print strings in a
-- readable manner.
newtype NT = NT Text deriving (Ord, Eq, Generic, Show)

instance Pretty NT where
  pretty (NT t) = t

instance NFData NT

-- | An algebraic data structure which can contain a terminal or a nonterminal.
data Symbol nt t
  = Nonterminal nt
  | Terminal t
  deriving (Ord, Eq, Read, Functor, Generic, Show)

instance (NFData t, NFData nt) => NFData (Symbol nt t)

instance (Pretty nt, Pretty t) => Pretty (Symbol nt t) where
  pretty (Nonterminal nt) = pretty nt
  pretty (Terminal t) = pretty t

-- | Bifunctor for symbol where first is the Nonterminal and second is Terminal.
instance Bifunctor Symbol where
  first f (Nonterminal nt) = Nonterminal $ f nt
  first _ (Terminal t) = Terminal t
  second _ (Nonterminal nt) = Nonterminal nt
  second f (Terminal t) = Terminal $ f t

-- | An algebraic data structure which describes a production.
data Production nt t = Production {prodLHS :: nt, prodRHS :: [Symbol nt t]}
  deriving (Ord, Eq, Read, Functor, Generic, Show)

instance (NFData t, NFData nt) => NFData (Production nt t)

instance (Pretty nt, Pretty t) => Pretty (Production nt t) where
  pretty (Production nt syms) =
    pretty nt <> " = " <> Text.intercalate " " (map pretty syms)

-- | Bifunctor for production where first is the Nonterminal and second is
-- Terminal.
instance Bifunctor Production where
  first f (Production nt s) = Production (f nt) (first f <$> s)
  second f (Production nt s) = Production nt (second f <$> s)

-- | Given Nonterminal return the value inside the Nonterminal.
unpackNonterminal :: Symbol nt t -> nt
unpackNonterminal (Nonterminal a) = a
unpackNonterminal (Terminal _) = error "Not a nonterminal."

-- | Given Terminal return the value inside the Terminal.
unpackTerminal :: Symbol nt t -> t
unpackTerminal (Terminal a) = a
unpackTerminal (Nonterminal _) = error "Not a terminal."

-- | Returns the right hand side of the production.
symbols :: Production nt t -> [Symbol nt t]
symbols (Production _ s) = s

-- | Returns the left hand side of the production.
nonterminal :: Production nt t -> nt
nonterminal (Production nt _) = nt

-- | Record used to store the 4-tuple that defines a context-free grammar.
data Grammar nt t = Grammar
  { start :: nt,
    terminals :: [t],
    nonterminals :: [nt],
    productions :: [Production nt t]
  }
  deriving (Ord, Eq, Show, Read, Generic)

instance (NFData t, NFData nt) => NFData (Grammar nt t)

instance Functor (Grammar nt) where
  fmap f (Grammar s ts nts ps) =
    Grammar s (f <$> ts) nts (fmap f <$> ps)

instance Bifunctor Grammar where
  bimap f g (Grammar s ts nts ps) =
    Grammar (f s) (g <$> ts) (f <$> nts) (bimap f g <$> ps)

-- | Finds a grammar by its left handside.
findProductions :: (Eq nt) => Grammar nt t -> nt -> [Production nt t]
findProductions grammar nt = filterNt $ productions grammar
  where
    filterNt = filter ((== nt) . nonterminal)

-- | Reverses a grammar by reversing the right handside of every production.
reverseGrammar :: Grammar nt t -> Grammar nt t
reverseGrammar grammar =
  grammar {productions = reverseProduction <$> productions grammar}
  where
    reverseProduction (Production nt s) = Production nt (reverse s)

-- | Augmenting the grammar corresponds to the augmentation in algorithm 8 of
-- the LLP paper.
augmentGrammar ::
  Grammar nt t ->
  Grammar (AugmentedNonterminal nt) (AugmentedTerminal t)
augmentGrammar grammar =
  grammar
    { start = Start,
      terminals = terminals',
      nonterminals = nonterminals',
      productions = productions'
    }
  where
    augmented_productions = augmentProduction <$> productions grammar
    productions' = augmented_productions ++ [Production Start symbols']
    nonterminals' = (AugmentedNonterminal <$> nonterminals grammar) ++ [Start]
    augmented_terminals = AugmentedTerminal <$> terminals grammar
    terminals' = augmented_terminals ++ [RightTurnstile, LeftTurnstile]
    start' = Nonterminal . AugmentedNonterminal $ start grammar
    leftPad = [Terminal RightTurnstile]
    rightPad = [Terminal LeftTurnstile]
    symbols' = leftPad ++ [start'] ++ rightPad
    augmentProduction = bimap AugmentedNonterminal AugmentedTerminal

-- | Predicate used to tell if a symbol is a terminal.
isTerminal :: Symbol nt t -> Bool
isTerminal (Terminal _) = True
isTerminal (Nonterminal _) = False

-- | Predicate used to tell if a symbol is a nonterminal.
isNonterminal :: Symbol nt t -> Bool
isNonterminal = not . isTerminal

-- | Creates  map for productions such that the nonterminals maps to possible
-- right handsides.
toProductionsMap ::
  (Ord nt, Ord t) =>
  [Production nt t] ->
  Map nt [[Symbol nt t]]
toProductionsMap = auxiliary Map.empty
  where
    auxiliary prod_map [] = prod_map
    auxiliary prod_map ((Production nt s) : as)
      | nt `Map.member` prod_map = auxiliary new_prod_map as
      | otherwise = auxiliary new_prod_map' as
      where
        new_prod_map = Map.adjust (s :) nt prod_map
        new_prod_map' = Map.insert nt [s] prod_map

-- | Given a grammar using NT and T convert it to a grammar using strings.
unpackNTTGrammar :: Grammar NT T -> Grammar String String
unpackNTTGrammar grammar =
  grammar
    { start = unpackNT $ start grammar,
      terminals = unpackT <$> terminals grammar,
      nonterminals = unpackNT <$> nonterminals grammar,
      productions = bimap unpackNT unpackT <$> productions grammar
    }
  where
    unpackT (T s) = Text.unpack s
    unpackT (TLit s) = Text.unpack s
    unpackNT (NT s) = Text.unpack s

-- | Given a string of symbols, find all the nonterminals and make tuples where
-- each nonterminal is the first element of the tuple and the second element is
-- the symbols which comes after that nonterminal.
rightSymbols :: [Symbol nt t] -> [(nt, [Symbol nt t])]
rightSymbols [] = []
rightSymbols ((Terminal _) : xs) = rightSymbols xs
rightSymbols ((Nonterminal x) : xs) = (x, xs) : rightSymbols xs

grammarDuplicates :: (Ord t, Ord nt) => Grammar nt t -> ([nt], [t], [Production nt t])
grammarDuplicates grammar = (hasDuplicates nts, hasDuplicates ts, hasDuplicates ps)
  where
    nts = nonterminals grammar
    ts = terminals grammar
    ps = productions grammar

hasDuplicates :: (Ord a) => [a] -> [a]
hasDuplicates = Set.toList . auxiliary Set.empty Set.empty
  where
    auxiliary dups _ [] = dups
    auxiliary dups visited (x : xs)
      | x `Set.member` visited = auxiliary new_dups new_visited xs
      | otherwise = auxiliary dups new_visited xs
      where
        new_visited = Set.insert x visited
        new_dups = Set.insert x dups

grammarError :: (Ord nt, Ord t, Show nt, Show t) => Grammar nt t -> Maybe Text
grammarError grammar
  | not $ null nt_dups = Just $ Text.pack [i|The given grammar contains duplicate nonterminals because of #{nt_dups_str}.|]
  | not $ null t_dups = Just $ Text.pack [i|The given grammar contains duplicate terminals because of #{t_dups_str}.|]
  | not $ null p_dups = Just $ Text.pack [i|The given grammar contains duplicate productions because of #{p_dups_str}.|]
  | not $ null nonproductive = Just $ Text.pack [i|The given grammar contains nonproductive productions due to the following nonterminals #{nonproductive_str}.|]
  | otherwise = Nothing
  where
    nts = Set.fromList $ nonterminals grammar
    nonproductive = nts `Set.difference` closure grammar
    nonproductive_str = List.intercalate ", " . fmap show $ Set.toList nonproductive
    (nt_dups, t_dups, p_dups) = grammarDuplicates grammar
    nt_dups_str = List.intercalate ", " . fmap show $ nt_dups
    t_dups_str = List.intercalate ", " . fmap show $ t_dups
    p_dups_str = List.intercalate ", " $ fmap show p_dups

-- https://zerobone.net/blog/cs/non-productive-cfg-rules/
closure :: (Ord nt, Ord t) => Grammar nt t -> Set nt
closure grammar = fixedPointIterate (==) (`newProductives` prods) Set.empty
  where
    prods = productions grammar
    isProductive1 set (Nonterminal nt) = nt `Set.member` set
    isProductive1 _ (Terminal _) = True
    isProductive set = all (isProductive1 set) . symbols
    newProductives set = Set.fromList . fmap nonterminal . List.filter (isProductive set)

reachable :: (Ord nt, Ord t) => Grammar nt t -> Set nt
reachable grammar = fixedPointIterate (==) f initial
  where
    prods_map =
      Map.map
        ( Set.fromList
            . fmap unpackNonterminal
            . concatMap (filter isNonterminal)
        )
        $ toProductionsMap
        $ productions grammar
    initial = Set.singleton $ start grammar
    prods = fromMaybe Set.empty . flip Map.lookup prods_map
    f prev = prev `Set.union` Set.unions (Set.map prods prev)

data Unused t
  = Unused
  | Used t
  deriving (Show, Eq, Ord, Generic)

instance (Pretty t) => Pretty (Unused t) where
  pretty (Used t) = pretty t
  pretty Unused = "⊥"

instance (NFData t) => NFData (Unused t)

newtype ParsingTerminals t
  = ParsingTerminals
  {pTerminals :: [Unused t]}
  deriving (Show, Eq, Ord)

parsingTerminals :: [t] -> ParsingTerminals t
parsingTerminals =
  ParsingTerminals
    . (++ [Unused])
    . fmap Used

getTerminals :: ParsingTerminals t -> [Unused t]
getTerminals = pTerminals

addUnusedTerminal ::
  Grammar nt t ->
  Grammar nt (Unused t)
addUnusedTerminal grammar =
  grammar
    { terminals = pTerminals $ parsingTerminals $ terminals grammar,
      productions = second Used <$> productions grammar
    }

extendByTerminals ::
  Grammar nt t ->
  Grammar (Symbol nt t) t
extendByTerminals grammar = new_grammar
  where
    ts = terminals grammar
    nts = nonterminals grammar
    left_nts = map Nonterminal nts
    right_nts = map Terminal ts
    new_nts = right_nts ++ left_nts
    ts_prods =
      zipWith Production right_nts $
        map (List.singleton . Terminal) ts
    toSymbol (Production nt syms) =
      Production (Nonterminal nt) $ Nonterminal <$> syms
    nts_prods = toSymbol <$> productions grammar
    new_grammar =
      Grammar
        { start = Nonterminal $ start grammar,
          terminals = ts,
          nonterminals = new_nts,
          productions = nts_prods ++ ts_prods
        }

data ParsingGrammar nt t
  = ParsingGrammar
  { pProductionNames :: IntMap Text,
    pGrammar :: Grammar (AugmentedNonterminal (Symbol nt t)) (AugmentedTerminal (Unused t))
  }
  deriving (Show, Eq, Ord)

getNames :: ParsingGrammar nt t -> IntMap Text
getNames = pProductionNames

getGrammar :: ParsingGrammar nt t -> Grammar (AugmentedNonterminal (Symbol nt t)) (AugmentedTerminal (Unused t))
getGrammar = pGrammar

parsingGrammar :: IntMap Text -> Grammar nt t -> ParsingGrammar nt t
parsingGrammar production_names =
  ParsingGrammar production_names
    . augmentGrammar
    . addUnusedTerminal
    . extendByTerminals

pickN :: Int -> [a] -> Gen [a]
pickN n xs = do
  indices <- vectorOf n $ choose (0, length xs - 1)
  pure [xs !! j | j <- indices]

pickOne :: [a] -> Gen a
pickOne xs = do
  j <- choose (0, length xs - 1)
  pure $ xs !! j

genProduction :: [t] -> [nt] -> Gen (Production nt t)
genProduction ts nts = sized $ \j -> do
  k <- choose (0, j `div` 5)
  Production
    <$> pickOne nts
    <*> pickProductionSymbols k ts nts

pickProductionSymbols :: Int -> [t] -> [nt] -> Gen [Symbol nt t]
pickProductionSymbols 0 _ _ = pure []
pickProductionSymbols k ts nts =
  (:)
    <$> pickOne (Terminal <$> ts)
    <*> pickN (k - 1) ((Terminal <$> ts) <> (Nonterminal <$> nts))

genNtProduction :: [t] -> [nt] -> nt -> Gen (Production nt t)
genNtProduction ts nts nt = sized $ \j -> do
  k <- choose (0, j `div` 5)
  Production
    <$> pure nt
    <*> pickProductionSymbols k ts nts

genGrammar :: (Ord nt, Ord t) => Int -> [nt] -> [t] -> Gen (Grammar nt t)
genGrammar k nts ts = do
  g <- grammar
  if nts_set == closure g && nts_set == reachable g
    then pure g
    else genGrammar k nts ts
  where
    nts_set = Set.fromList nts
    n = length nts
    grammar =
      Grammar
        <$> pickOne nts
        <*> pure ts
        <*> pure nts
        <*> ( (<>)
                <$> mapM (genNtProduction ts nts) nts
                <*> vectorOf (k - n) (genProduction ts nts)
            )

intToAlpha :: Int -> Text
intToAlpha n =
  if n < 26
    then Text.singleton a
    else Text.snoc (intToAlpha (n `div` 26 - 1)) a
  where
    a = chr (ord 'A' + (n `mod` 26))

genNonterminals :: Int -> [NT]
genNonterminals k =
  NT . intToAlpha <$> [0 .. max 0 (k - 1)]
