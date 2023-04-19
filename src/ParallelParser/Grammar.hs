module ParallelParser.Grammar
  ( Grammar (..),
    Symbol (..),
    Production (..),
    AugmentedNonterminal (..),
    AugmentedTerminal (..),
    ExtendedNonterminal (..),
    ExtendedTerminal (..),
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
    unextendNT,
    unextendT,
    extendGrammar
  )
where

import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Composition
import qualified Data.List as List
import Data.Map (Map (..))
import qualified Data.Map as Map hiding (Map (..))
import Data.Maybe
import Data.Set (Set (..))
import qualified Data.Set as Set
import Data.Tuple.Extra (both)
import Debug.Trace (traceShow)
import Text.ParserCombinators.ReadP

debug x = traceShow x x

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

toSymbol :: (Read nt, Read t) => [String] -> [String] -> String -> Symbol nt t
toSymbol ts nts symbol
  | symbol `elem` nts = Nonterminal $ read symbol
  | symbol `elem` ts = Terminal $ read symbol
  | otherwise = error $ show symbol ++ " is not a defined symbol."

replaceEscapedChars :: String -> String
replaceEscapedChars "" = ""
replaceEscapedChars input@(x : xs)
  | x == '\\' = start ++ replaceEscapedChars xs'
  | otherwise = x : replaceEscapedChars xs
  where
    (start, xs') = auxiliary input
    auxiliary ('\\' : '\\' : ys) = ("\\", ys)
    auxiliary ('\\' : ',' : ys) = (",", ys)
    auxiliary ('\\' : '}' : ys) = ("}", ys)
    auxiliary ('\\' : 't' : ys) = ("\t", ys)
    auxiliary ('\\' : 'r' : ys) = ("\r", ys)
    auxiliary ('\\' : 'n' : ys) = ("\n", ys)
    auxiliary ('\\' : 's' : ys) = (" ", ys)
    auxiliary ('\\' : ys) = ("", ys)
    auxiliary ys = ("", ys)

setElement = replaceEscapedChars . concat <$> many1 escaped
  where
    escaped = string "\\}" <++ string "\\," <++ fmap List.singleton (satisfy (`notElem` [' ', ',', '}', '\n', '\r', '\t']))

pGrammar :: (Read nt, Read t) => ReadP (Grammar nt t)
pGrammar = tuple
  where
    set = sepBySkip setElement sep
    production_set ts nts = sepBySkip (production ts nts) sep
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

data ExtendedTerminal t
  = ExtendedTerminal t
  | End
  deriving (Ord, Eq)

instance Show t => Show (ExtendedTerminal t) where
  show End = "End"
  show (ExtendedTerminal t) = show t

data ExtendedNonterminal nt
  = ExtendedNonterminal nt
  | ExtendedStart
  deriving (Ord, Eq)

instance Show nt => Show (ExtendedNonterminal nt) where
  show ExtendedStart = "Start"
  show (ExtendedNonterminal t) = show t

unextendNT :: ExtendedNonterminal nt -> nt
unextendNT (ExtendedNonterminal nt) = nt

unextendT :: ExtendedTerminal t -> t
unextendT (ExtendedTerminal t) = t

extendGrammar ::
  Int ->
  Grammar nt t ->
  (Grammar (ExtendedNonterminal nt) (ExtendedTerminal t),
   [ExtendedTerminal t])
extendGrammar k grammar =
  (grammar
    { start = ExtendedStart,
      terminals = terminals',
      nonterminals = nonterminals',
      productions = productions'
    }, padding)
  where
    extended_productions = augmentProduction <$> productions grammar
    productions' = Production ExtendedStart symbols' : extended_productions
    nonterminals' = ExtendedStart : (ExtendedNonterminal <$> nonterminals grammar)
    extended_terminals = ExtendedTerminal <$> terminals grammar
    terminals' = End : extended_terminals
    start' = Nonterminal . ExtendedNonterminal $ start grammar
    symbols' = start' : (Terminal <$> padding)
    padding = replicate k End
    augmentProduction = bimap ExtendedNonterminal ExtendedTerminal

augmentGrammar ::
  Int ->
  Int ->
  Grammar nt t ->
  Grammar (AugmentedNonterminal nt) (AugmentedTerminal t)
augmentGrammar q k grammar =
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
    symbols' = replicate q (Terminal RightTurnstile) ++ [start'] ++ replicate k (Terminal LeftTurnstile)
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


