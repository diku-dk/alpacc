module Alpacc.Random
  ( random,
  )
where

import Alpacc.CFG
import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Data.Map qualified as Map
import Data.Text (Text)
import Test.QuickCheck
  ( generate,
  )

random :: Int -> Int -> Int -> IO Text
random num_terminals num_nonterminals num_productions = do
  spec <- generate (genDfaLexerSpec num_terminals)
  let ts = Map.keys $ regexMap spec
      nts = genNonterminals num_nonterminals
  grammar <- generate (genGrammar num_productions nts ts)
  pure $
    printDfaSpec spec
      <> "\n"
      <> printGrammar grammar
