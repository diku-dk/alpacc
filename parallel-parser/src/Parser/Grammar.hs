{-# OPTIONS_GHC -Wno-missing-fields #-}
module Parser.Grammar
  ( Grammar (..),
    Symbol(..),
    Production(..),
    T(..),
    NT(..),
    symbols,
    nonterminal,
    reverseGrammar
  )
where

import Data.Bifunctor (Bifunctor (first, second))
import Data.Char as C
import Data.Composition
import Data.Maybe
import Debug.Trace (traceShow)
import Text.ParserCombinators.ReadP

debug x = traceShow ("DEBUG: " ++ show x) x

newtype T = T String deriving (Ord, Eq)

instance Read T where
  readsPrec _ a = [(T a, "")]

instance Show T where
  show (T a) = a

newtype NT = NT String deriving (Ord, Eq)

instance Show NT where
  show (NT a) = a

instance Read NT where
  readsPrec _ a = [(NT a, "")]

data Symbol nt t = Nonterminal nt | Terminal t deriving (Ord, Eq, Show, Read)
data Production nt t = Production nt [Symbol nt t] deriving (Ord, Eq, Show, Read)

symbols :: Production nt t -> [Symbol nt t]
symbols (Production _ s) = s

nonterminal :: Production nt t -> nt
nonterminal (Production nt _) = nt

data (Show nt, Show t) => Grammar nt t = Grammar
  { start :: nt,
    terminals :: [t],
    nonterminals :: [nt],
    productions :: [Production nt t]
  }
  deriving (Show)


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
set = between (char '{') (char '}') (sepBySkip (munch1 (`notElem` [',', '}'])) sep)

toSymbol :: (Read nt, Read t) => [String] -> [String] -> String -> Symbol nt t
toSymbol ts nts symbol
  | symbol `elem` nts = Nonterminal $ read symbol
  | symbol `elem` ts = Terminal $ read symbol
  | otherwise = error $ show symbol ++ " is not a defined symbol."

pGrammar :: (Read nt, Read t, Show nt, Show t) => ReadP (Grammar nt t)
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

instance (Read nt, Read t, Show nt, Show t) => Read (Grammar nt t) where
  readsPrec _ = readP_to_S pGrammar

reverseGrammar :: (Show nt, Show t) => Grammar nt t -> Grammar nt t
reverseGrammar grammar = grammar {productions = reverseProduction <$> productions grammar}
  where
    reverseProduction (Production nt s) = Production nt (reverse s)