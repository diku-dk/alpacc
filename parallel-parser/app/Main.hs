module Main where


import Parser.Grammar 
import Parser.Parsing ( nullables, firsts, constraints, follows, last, before ) 
import qualified Data.Map as M
import Prelude hiding (last)

main :: IO ()
main = do
  contents <- getContents
  let grammar = read contents :: Grammar
  print . zip (productions grammar) . nullables $ grammar
  print $ constraints 2 grammar
  print . zip (productions grammar) $ firsts 2 grammar
  print . M.toList $ follows 1 grammar
  print . zip (productions grammar) $ last 2 grammar . symbols <$> productions grammar
  print . zip (nonterminals grammar) $ before 2 grammar <$> nonterminals grammar