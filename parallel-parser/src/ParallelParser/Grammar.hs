module ParallelParser.Grammar
  ( Grammar (..),
    Symbol (..),
    Production (..),
    AugmentedNonterminal (..),
    AugmentedTerminal (..),
    T (..),
    NT (..),
    symbols,
    nonterminal,
    reverseGrammar,
    augmentGrammar,
    findProductions,
    isTerminal,
    isNonterminal,
    toProductionsMap,
    unpackNTTGrammar,
  )
where

import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Composition
import qualified Data.List as List
import Data.Map (Map (..))
import qualified Data.Map as Map hiding (Map (..))
import Data.Maybe
import Debug.Trace (traceShow)
import Text.ParserCombinators.ReadP

newtype T = T String deriving (Ord, Eq)

data AugmentedTerminal t
  = AugmentedTerminal t
  | RightTurnstile
  | LeftTurnstile
  deriving (Ord, Eq)

instance Show t => Show (AugmentedTerminal t) where
  show RightTurnstile = "⊢"
  show LeftTurnstile = "⊣"
  show (AugmentedTerminal t) = show t

data AugmentedNonterminal nt
  = AugmentedNonterminal nt
  | Start
  deriving (Ord, Eq)

instance Show nt => Show (AugmentedNonterminal nt) where
  show (AugmentedNonterminal nt) = show nt
  show Start = "⊥"

instance Read T where
  readsPrec _ a = [(T a, "")]

instance Show T where
  show (T a) = a

newtype NT = NT String deriving (Ord, Eq)

instance Show NT where
  show (NT a) = a

instance Read NT where
  readsPrec _ a = [(NT a, "")]

data Symbol nt t
  = Nonterminal nt
  | Terminal t
  deriving (Ord, Eq, Show, Read, Functor)

instance Bifunctor Symbol where
  first f (Nonterminal nt) = Nonterminal $ f nt
  first _ (Terminal t) = Terminal t
  second _ (Nonterminal nt) = Nonterminal nt
  second f (Terminal t) = Terminal $ f t

data Production nt t
  = Production nt [Symbol nt t]
  deriving (Ord, Eq, Show, Read, Functor)

instance Bifunctor Production where
  first f (Production nt s) = Production (f nt) (first f <$> s)
  second f (Production nt s) = Production nt (second f <$> s)

symbols :: Production nt t -> [Symbol nt t]
symbols (Production _ s) = s

nonterminal :: Production nt t -> nt
nonterminal (Production nt _) = nt

data Grammar nt t = Grammar
  { start :: nt,
    terminals :: [t],
    nonterminals :: [nt],
    productions :: [Production nt t]
  }
  deriving (Ord, Eq, Show)

skipWhiteSpaces :: ReadP ()
skipWhiteSpaces = do
  _ <- munch (`elem` [' ', '\n', '\r', '\t'])
  return ()

skipSpacesAround :: ReadP a -> ReadP a
skipSpacesAround a = do
  _ <- skipWhiteSpaces
  result <- a
  _ <- skipWhiteSpaces
  return result

sep :: ReadP ()
sep = do
  _ <- skipSpacesAround (char ',')
  return ()

sepBySkip :: ReadP a -> ReadP sep -> ReadP [a]
sepBySkip a sep' = skipSpacesAround $ sepBy a sep'

set :: ReadP [String]
set = between (char '{') (char '}') (sepBySkip munch1_non_set sep)
  where
    munch1_non_set = munch1 (`notElem` [',', '}'])

toSymbol :: (Read nt, Read t) => [String] -> [String] -> String -> Symbol nt t
toSymbol ts nts symbol
  | symbol `elem` nts = Nonterminal $ read symbol
  | symbol `elem` ts = Terminal $ read symbol
  | otherwise = error $ show symbol ++ " is not a defined symbol."

pGrammar :: (Read nt, Read t) => ReadP (Grammar nt t)
pGrammar = tuple
  where
    set = sepBySkip setElement sep
    production_set ts nts = sepBySkip (production ts nts) sep
    setElement = munch1 (`notElem` [' ', ',', '}', '\n', '\r', '\t'])
    tupleElement = munch1 (`notElem` [' ', ',', ')', '\n', '\r', '\t'])

    production ts nts = do
      nt <- setElement
      _ <- skipSpacesAround $ string "->"
      symbols <- sepBySkip setElement (many1 (char ' '))
      return $ Production (read nt) (toSymbol ts nts <$> symbols)

    tuple = between (char '(') (char ')') $ do
      _ <- skipWhiteSpaces
      s <- tupleElement
      _ <- sep
      ts <- between (char '{') (char '}') set
      _ <- sep
      nts <- between (char '{') (char '}') set
      _ <- sep
      ps <- between (char '{') (char '}') (production_set ts nts)
      _ <- skipWhiteSpaces
      return
        Grammar
          { start = read s,
            terminals = read <$> ts,
            nonterminals = read <$> nts,
            productions = ps
          }

instance (Read nt, Read t) => Read (Grammar nt t) where
  readsPrec _ = readP_to_S pGrammar

findProductions :: (Eq b) => Grammar b t -> b -> [Production b t]
findProductions grammar nt = filterNt $ productions grammar
  where
    filterNt = filter ((== nt) . nonterminal)

reverseGrammar :: Grammar nt t -> Grammar nt t
reverseGrammar grammar =
  grammar {productions = reverseProduction <$> productions grammar}
  where
    reverseProduction (Production nt s) = Production nt (reverse s)

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
    productions' = Production Start symbols' : augmented_productions
    nonterminals' = Start : (AugmentedNonterminal <$> nonterminals grammar)
    augmented_terminals = AugmentedTerminal <$> terminals grammar
    terminals' = RightTurnstile : LeftTurnstile : augmented_terminals
    start' = Nonterminal . AugmentedNonterminal $ start grammar
    symbols' = [Terminal RightTurnstile, start', Terminal LeftTurnstile]
    augmentProduction = bimap AugmentedNonterminal AugmentedTerminal

isTerminal :: Symbol nt t -> Bool
isTerminal (Terminal _) = True
isTerminal (Nonterminal _) = False

isNonterminal :: Symbol nt t -> Bool
isNonterminal = not . isTerminal

toProductionsMap ::
  (Ord nt, Ord t) =>
  [Production nt t] ->
  Map nt [[Symbol nt t]]
toProductionsMap = Map.fromList . fmap toPair . groupSort
  where
    groupSort = List.groupBy nonterminalEq . List.sort
    nonterminalEq a b = nonterminal a == nonterminal b
    toPair a = (nonterminal $ head a, symbols <$> a)

unpackNTTGrammar :: Grammar NT T -> Grammar String String
unpackNTTGrammar grammar =
  grammar
    { start = unpackNT $ start grammar,
      terminals = unpackT <$> terminals grammar,
      nonterminals = unpackNT <$> nonterminals grammar,
      productions = bimap unpackNT unpackT <$> productions grammar
    }
  where
    unpackT (T s) = s
    unpackNT (NT s) = s
