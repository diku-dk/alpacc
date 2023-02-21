module MyLib (
  Terminal (..),
  Nonterminal (..),
  Grammar (..),
  ExtendedGrammar (..),
  extendedGrammarToGrammar) where

import Control.Monad (join)
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Bifunctor (Bifunctor (first, second))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Char as C
import Data.Composition
import Data.Maybe
import Debug.Trace (traceShow)
import GHC.Generics (Generic)
import Text.ParserCombinators.ReadP

debug x = traceShow ("DEBUG: " ++ show x) x

newtype Terminal = Terminal String deriving (Show)

instance Read Terminal where
  readsPrec _ a = [(Terminal a, "")]

newtype Nonterminal = Nonterminal String deriving (Show)

instance Read Nonterminal where
  readsPrec _ a = [(Nonterminal a, "")]

data Production = Production Nonterminal [Either Nonterminal Terminal] deriving (Show)

data Grammar = Grammar
  { start :: Nonterminal,
    terminals :: [Terminal],
    nonterminals :: [Nonterminal],
    productions :: [Production]
  }
  deriving (Show)

toProduction :: [String] -> [String] -> String -> Either Nonterminal Terminal
toProduction ts nts symbol
  | symbol `elem` nts = Left $ read symbol
  | symbol `elem` ts = Right $ read symbol
  | otherwise = error "test"

elem' :: ReadP String
elem' = munch1 C.isAlphaNum

sep :: ReadP ()
sep = do
  _ <- skipSpaces
  _ <- char ','
  skipSpaces

sepBySkip :: ReadP a -> ReadP sep -> ReadP [a]
sepBySkip a sep' = do
  _ <- skipSpaces
  result <- sepBy a sep'
  _ <- skipSpaces
  return result

pGrammar :: ReadP Grammar
pGrammar = tuple
  where
    set = sepBySkip elem' sep
    production_set ts nts = sepBySkip (production ts nts) sep

    production ts nts = do
      nt <- elem'
      _ <- skipSpaces
      arrow <- string "->"
      _ <- skipSpaces
      symbols <- sepBySkip elem' (many1 (char ' '))
      return $ Production (read nt) (toProduction ts nts <$> symbols)

    tuple = between (char '(') (char ')') $ do
      _ <- skipSpaces
      s <- elem'
      _ <- sep
      ts <- between (char '{') (char '}') set
      _ <- sep
      nts <- between (char '{') (char '}') set
      _ <- sep
      ps <- between (char '{') (char '}') (production_set ts nts)
      _ <- skipSpaces
      return
        Grammar
          { start = read s,
            terminals = read <$> ts,
            nonterminals = read <$> nts,
            productions = ps
          }

instance Read Grammar where
  readsPrec _ = readP_to_S pGrammar

data ExtendedGrammar = ExtendedGrammar
  { extendedStart :: Nonterminal,
    extendedEnd :: Terminal,
    grammar :: Grammar
  }
  deriving (Show)

instance Read ExtendedGrammar where
  readsPrec _ = readP_to_S tuple
    where
      tuple = between (char '(') (char ')') $ do
        _ <- skipSpaces
        extended_start <- elem'
        _ <- sep
        extended_end <- elem'
        _ <- sep
        grammar' <- pGrammar
        _ <- skipSpaces
        return 
          ExtendedGrammar {
            extendedStart = read extended_start,
            extendedEnd = read extended_end,
            grammar = grammar'
          }

extendedGrammarToGrammar :: ExtendedGrammar -> Grammar
extendedGrammarToGrammar extended_grammar = Grammar {
  start = extended_start,
  terminals = terminals',
  nonterminals = nonterminals',
  productions = productions' 
}
  where
    extended_end = extendedEnd extended_grammar
    extended_start = extendedStart extended_grammar
    grammar' = grammar extended_grammar
    start' = start grammar'
    terminals' = extended_end : terminals grammar'
    nonterminals' = extended_start : nonterminals grammar'
    productions' = extended_production : productions grammar'
    extended_production = Production extended_start [Left start', Right extended_end]