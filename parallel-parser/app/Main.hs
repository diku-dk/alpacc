module Main where

import Parser.Grammar 
import Parser.Parsing 

main :: IO ()
main = do
  contents <- getContents
  -- print . nullable . extendedGrammarToGrammar $ (read contents :: ExtendedGrammar)
  let grammar = read contents :: Grammar
  print . zip (productions grammar) . nullables $ grammar
  print . zip (productions grammar) . firsts $ grammar
  print $ constraints grammar
  print $ follows grammar