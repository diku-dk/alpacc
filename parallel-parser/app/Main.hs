module Main where


import Parser.Grammar
import Parser.Parsing ( nullable, constraints, follows, last, before, firstsMaxK, first )
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
  print grammar
  let nullable' = nullable grammar
  mapM_ print $ (\(Production nt s) -> (nt, s, nullable' s)) <$> productions grammar
  print $ constraints 2 grammar
  print . M.toList $ follows 1 grammar
  let first' = first 5 grammar
  mapM_ print $ (\(Production nt s) -> (nt, s, first' s)) <$> productions grammar
  print . zip (productions grammar) $ last 2 grammar . symbols <$> productions grammar
  print . zip (nonterminals grammar) $ before 2 grammar <$> nonterminals grammar