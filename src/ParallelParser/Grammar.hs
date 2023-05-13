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
    extendGrammar,
    unpackNonterminal,
    unpackTerminal
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
import Control.DeepSeq
import GHC.Generics

debug x = traceShow x x

-- | Structure used for terminals making it easier to print strings in a
-- readable manner.
newtype T = T String deriving (Ord, Eq)

-- | Used in augmenting terminals for the augmented grammar.
data AugmentedTerminal t
  = AugmentedTerminal t
  | RightTurnstile
  | LeftTurnstile
  deriving (Ord, Eq, Generic)

instance NFData t => NFData (AugmentedTerminal t)

-- | Prints whats inside the augmented terminal structure.
instance Show t => Show (AugmentedTerminal t) where
  show RightTurnstile = "⊢"
  show LeftTurnstile = "⊣"
  show (AugmentedTerminal t) = show t

-- | Used in augmenting nonterminals for the augmented grammar.
data AugmentedNonterminal nt
  = AugmentedNonterminal nt
  | Start
  deriving (Ord, Eq, Generic)

instance NFData nt => NFData (AugmentedNonterminal nt)

-- | Prints whats inside the augmented nonterminal structure.
instance Show nt => Show (AugmentedNonterminal nt) where
  show (AugmentedNonterminal nt) = show nt
  show Start = "⊥"

-- | Reads the string as it is for the terminal.
instance Read T where
  readsPrec _ a = [(T a, "")]

-- | Prints the string inside the terminal symbol.
instance Show T where
  show (T a) = a

-- | Structure used for nonterminals making it easier to print strings in a
-- readable manner.
newtype NT = NT String deriving (Ord, Eq)

-- | Reads the string as it is for the nonterminal.
instance Show NT where
  show (NT a) = a

-- | Reads the string as it is for the terminal.
instance Read NT where
  readsPrec _ a = [(NT a, "")]

-- | An algebraic data structure which can contain a terminal or a nonterminal.
data Symbol nt t
  = Nonterminal nt
  | Terminal t
  deriving (Ord, Eq, Read, Functor, Generic)

instance (NFData t, NFData nt) => NFData (Symbol nt t)

-- | Shows whats inside the symbol.
instance (Show nt, Show t) => Show (Symbol nt t) where
  show (Nonterminal a) = show a
  show (Terminal a) = show a

-- | Bifunctor for symbol where first is the Nonterminal and second is Terminal.
instance Bifunctor Symbol where
  first f (Nonterminal nt) = Nonterminal $ f nt
  first _ (Terminal t) = Terminal t
  second _ (Nonterminal nt) = Nonterminal nt
  second f (Terminal t) = Terminal $ f t

-- | An algebraic data structure which describes a production.
data Production nt t
  = Production nt [Symbol nt t]
  deriving (Ord, Eq, Show, Read, Functor, Generic)

instance (NFData t, NFData nt) => NFData (Production nt t)

-- | Bifunctor for production where first is the Nonterminal and second is
-- Terminal.
instance Bifunctor Production where
  first f (Production nt s) = Production (f nt) (first f <$> s)
  second f (Production nt s) = Production nt (second f <$> s)

-- | Given Nonterminal return the value inside the Nonterminal.
unpackNonterminal :: Symbol nt t -> nt
unpackNonterminal (Nonterminal a) = a

-- | Given Terminal return the value inside the Terminal.
unpackTerminal :: Symbol nt t -> t
unpackTerminal (Terminal a) = a

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
  deriving (Ord, Eq, Show, Generic)

instance (NFData t, NFData nt) => NFData (Grammar nt t)

-- | Skips white spaces.
skipWhiteSpaces :: ReadP ()
skipWhiteSpaces = do
  _ <- munch (`elem` [' ', '\n', '\r', '\t'])
  return ()

-- | Removes the white spaces around som ReadP term.
skipSpacesAround :: ReadP a -> ReadP a
skipSpacesAround a = do
  _ <- skipWhiteSpaces
  result <- a
  _ <- skipWhiteSpaces
  return result

-- | Removes whitespaces around a comma.
sep :: ReadP ()
sep = do
  _ <- skipSpacesAround (char ',')
  return ()

-- | Parses all elements seperated by a comma and ignore spaces.
sepBySkip :: ReadP a -> ReadP sep -> ReadP [a]
sepBySkip a sep' = skipSpacesAround $ sepBy a sep'

-- | Given a string parses it as a symbol.
toSymbol :: (Read nt, Read t) => [String] -> [String] -> String -> Symbol nt t
toSymbol ts nts symbol
  | symbol `elem` nts = Nonterminal $ read symbol
  | symbol `elem` ts = Terminal $ read symbol
  | otherwise = error $ show symbol ++ " is not a defined symbol."

-- | Replaces escaped characters with their counter parts.
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

-- | Parses a elements in a set as a string and accounts for escape characters.
setElement :: ReadP String
setElement = replaceEscapedChars . concat <$> many1 escaped
  where
    whitespace_list = [' ', ',', '}', '\n', '\r', '\t']
    whitespace = fmap List.singleton (satisfy (`notElem` whitespace_list))
    escaped = string "\\}" <++ string "\\," <++ whitespace

-- | Parses a string as a grammar.
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

-- | Uses pGrammar to allow for reading the grammar.
instance (Read nt, Read t) => Read (Grammar nt t) where
  readsPrec _ = readP_to_S pGrammar

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

-- | Extends the terminals by one terminal which can be used when constructing
-- the follow sets.
data ExtendedTerminal t
  = ExtendedTerminal t
  | End
  deriving (Ord, Eq)

-- | Shows whats inside the terminals or the End terminal. 
instance Show t => Show (ExtendedTerminal t) where
  show End = "End"
  show (ExtendedTerminal t) = show t

-- | Extends the nonterminals by one nonterminal which can be used when constructing
-- the follow sets.
data ExtendedNonterminal nt
  = ExtendedNonterminal nt
  | ExtendedStart
  deriving (Ord, Eq)

-- | Shows whats inside the nonterminals or the Start nonterminal. 
instance Show nt => Show (ExtendedNonterminal nt) where
  show ExtendedStart = "Start"
  show (ExtendedNonterminal t) = show t

-- | Given ExtendedNonterminal return the value inside the Nonterminal.
unextendNT :: ExtendedNonterminal nt -> nt
unextendNT (ExtendedNonterminal nt) = nt

-- | Given ExtendedNonterminal return the value inside the ExtendedNonterminal.
unextendT :: ExtendedTerminal t -> t
unextendT (ExtendedTerminal t) = t

-- | Extends a grammar with a new starting production where the old starting
-- production is in the beginning of the left handside and k End terminals are
-- at the back of the left hand side.
extendGrammar ::
  Int ->
  Grammar nt t ->
  Grammar (ExtendedNonterminal nt) (ExtendedTerminal t)
extendGrammar k grammar =
  Grammar
    { start = ExtendedStart,
      terminals = terminals',
      nonterminals = nonterminals',
      productions = productions'
    }
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
    productions' = Production Start symbols' : augmented_productions
    nonterminals' = Start : (AugmentedNonterminal <$> nonterminals grammar)
    augmented_terminals = AugmentedTerminal <$> terminals grammar
    terminals' = RightTurnstile : LeftTurnstile : augmented_terminals
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
toProductionsMap = Map.fromList . fmap toPair . groupSort
  where
    groupSort = List.groupBy nonterminalEq . List.sort
    nonterminalEq a b = nonterminal a == nonterminal b
    toPair a = (nonterminal $ head a, symbols <$> a)

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
    unpackT (T s) = s
    unpackNT (NT s) = s
