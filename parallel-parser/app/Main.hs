module Main where

import Parser.Grammar 
import Parser.Parsing

main :: IO ()
main = do
  contents <- getContents
  -- print . nullable . extendedGrammarToGrammar $ (read contents :: ExtendedGrammar)
  let grammar = read contents :: Grammar
  print . zip (productions grammar) . nullable $ grammar
