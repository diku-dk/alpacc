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
    unpackTerminal,
    substringGrammar,
    rightSymbols,
    grammarDuplicates
  )
where

import Control.DeepSeq
import Data.Bifunctor (Bifunctor (bimap, first, second))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)
import qualified Data.Set as Set
import GHC.Generics
import Text.ParserCombinators.ReadP
    ( ReadP,
      (<++),
      between,
      char,
      many1,
      munch,
      munch1,
      readP_to_S,
      satisfy,
      sepBy,
      string )

-- import Debug.Trace (traceShow)
-- debug x = traceShow x x

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
  = Production { prodLHS :: nt, prodRHS :: [Symbol nt t] }
  deriving (Ord, Eq, Read, Functor, Generic)

instance (Show nt, Show t) => Show (Production nt t) where
  show (Production nt s) = show nt ++ " -> " ++ show s

instance (NFData t, NFData nt) => NFData (Production nt t)

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
  | x == '\\' = _start ++ replaceEscapedChars xs'
  | otherwise = x : replaceEscapedChars xs
  where
    (_start, xs') = auxiliary input
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
      _symbols <- sepBySkip setElement (many1 (char ' '))
      return $ Production (read nt) (toSymbol ts nts <$> _symbols)

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
unextendNT ExtendedStart = error "Cannot unextend Start."

-- | Given ExtendedNonterminal return the value inside the ExtendedNonterminal.
unextendT :: ExtendedTerminal t -> t
unextendT (ExtendedTerminal t) = t
unextendT End = error "Cannot unextend End."

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
    leftPad = replicate 1 $ Terminal RightTurnstile
    rightPad = replicate 1 $ Terminal LeftTurnstile
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
        new_prod_map = Map.adjust (s:) nt prod_map
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
    unpackT (T s) = s
    unpackNT (NT s) = s

data SubstringNonterminal nt
  = ExistingNT nt
  | ArbitraryNT Integer
  deriving (Eq, Ord, Show)

substringGrammar ::
  (Ord nt, Ord t, Show nt, Show t) =>
  Grammar nt t ->
  Grammar (SubstringNonterminal nt) t
substringGrammar grammar =
  grammar
    { nonterminals = new_nts,
      productions = new_prods,
      start = new_start
    }
  where
    nts' = ExistingNT <$> nonterminals grammar
    prods' = first ExistingNT <$> productions grammar
    prods_map = toProductionsMap prods'
    substr_prods_map = Map.mapKeys toAnt $ concatMap extraSubstrings <$> prods_map
    mapEntryToProds = Map.mapWithKey (\k v -> Production k <$> v)
    substr_prods = concat . Map.elems $ mapEntryToProds substr_prods_map
    to_substr_prods = Production new_start . List.singleton . Nonterminal <$> (nts' ++ Map.keys substr_prods_map)
    nt_to_ant_map = Map.fromList . zip nts' $ ArbitraryNT <$> [1 ..]
    toAnt = (nt_to_ant_map Map.!)
    new_start = ArbitraryNT 0
    new_prods = prods' ++ fmap firstChange substr_prods ++ to_substr_prods
    new_nts = new_start : (nts' ++ Map.keys substr_prods_map)
    extraSubstrings [] = []
    extraSubstrings s@(_x : xs) = s : extraSubstrings xs
    firstChange (Production nt ((Nonterminal x):xs)) = Production nt (Nonterminal (toAnt x):xs)
    firstChange a = a

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

hasDuplicates :: Ord a => [a] -> [a]
hasDuplicates = Set.toList . auxiliary Set.empty Set.empty
  where
    auxiliary dups _ [] = dups
    auxiliary dups visited (x:xs)
      | x `Set.member` visited = auxiliary new_dups new_visited xs
      | otherwise = auxiliary dups new_visited xs
      where
        new_visited = Set.insert x visited
        new_dups = Set.insert x dups
