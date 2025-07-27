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

chars :: Int -> [Char]
chars n = take n ['a' .. 'z']

random :: Int -> Int -> Int -> Int -> IO Text
random a b c d | a <= 0 || b <= 0 || c < 0 || d < 0 = pure ""
random num_chars num_terminals 0 _ =
  printDfaSpec <$> generate (genDfaLexerSpec num_terminals chrs)
  where
    chrs = chars num_chars
random num_chars num_terminals _ 0 =
  printDfaSpec <$> generate (genDfaLexerSpec num_terminals chrs)
  where
    chrs = chars num_chars
random num_chars num_terminals num_nonterminals num_productions = do
  spec <- generate (genDfaLexerSpec num_terminals chrs)
  let ts = Map.keys $ regexMap spec
      nts = genNonterminals num_nonterminals
  grammar <- generate (genGrammar num_productions nts ts)
  pure $ printDfaSpec spec <> "\n" <> printGrammar grammar
  where
    chrs = chars num_chars
