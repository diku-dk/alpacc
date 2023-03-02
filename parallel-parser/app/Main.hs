module Main where


import Parser.Grammar
import Parser.Parsing
import qualified Data.Map as M
import Prelude hiding (last)
import Text.ParserCombinators.ReadP
import qualified Data.List as L
import qualified Data.Char as C
import Data.Maybe
import Debug.Trace (traceShow)

debug x = traceShow ("DEBUG: " ++ show x) x

main :: IO ()
main = do
  contents <- getContents
  let grammar = read contents :: Grammar NT T
  let augmentedGrammar = augmentGrammar grammar
  let nullable' = nullable grammar
  let last' = last 2 augmentedGrammar
  -- mapM_ print $ (\(Production nt s) -> (nt, s, nullable' s)) <$> productions grammar
  let first' = first 5 grammar
  -- mapM_ print $ (\(Production nt s) -> (nt, s, first' s)) <$> productions grammar
  -- print . zip (productions grammar) $ last 2 grammar . symbols <$> productions grammar
  -- print . zip (nonterminals grammar) $ before 2 grammar <$> nonterminals grammar
  -- print $ (\(Production nt s) -> (nt, last' s)) <$> productions augmentedGrammar
  print . moveDots . production $ llpItems 1 1 grammar