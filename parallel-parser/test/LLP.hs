module LLP (tests) where

import Data.Composition
import Data.Function
import qualified Data.List as List
import qualified Data.Set as Set
import ParallelParser.Grammar
import ParallelParser.Parser
import Test.HUnit

grammar :: Grammar String String
grammar =
  Grammar
    { start = "E",
      terminals = ["[", "]", "+", "a"],
      nonterminals = ["E", "E'", "T"],
      productions =
        [ Production "E" [Nonterminal "T", Nonterminal "E'"],
          Production "E'" [Terminal "+", Nonterminal "T", Nonterminal "E'"],
          Production "E" [],
          Production "T" [Terminal "a"],
          Production "T" [Terminal "[", Nonterminal "E", Terminal "]"]
        ]
    }

augmentedGrammar :: Grammar (AugmentedNonterminal String) (AugmentedTerminal String)
augmentedGrammar =
  Grammar
    { start = Start,
      terminals = ([RightTurnstile, LeftTurnstile] ++) $ AugmentedTerminal <$> ["[", "]", "+", "a"],
      nonterminals = (Start :) $ AugmentedNonterminal <$> ["E", "E'", "T"],
      productions =
        [ Production
            Start
            [ Terminal RightTurnstile,
              Nonterminal (AugmentedNonterminal "E"),
              Terminal LeftTurnstile
            ],
          Production
            (AugmentedNonterminal "E")
            [ Nonterminal (AugmentedNonterminal "T"),
              Nonterminal (AugmentedNonterminal "E'")
            ],
          Production
            (AugmentedNonterminal "E'")
            [ Terminal (AugmentedTerminal "+"),
              Nonterminal (AugmentedNonterminal "T"),
              Nonterminal (AugmentedNonterminal "E'")
            ],
          Production (AugmentedNonterminal "E") [],
          Production
            (AugmentedNonterminal "T")
            [ Terminal (AugmentedTerminal "a")
            ],
          Production
            (AugmentedNonterminal "T")
            [ Terminal (AugmentedTerminal "["),
              Nonterminal (AugmentedNonterminal "E"),
              Terminal (AugmentedTerminal "]")
            ]
        ]
    }

augmentGrammarTestCase = TestCase $ assertEqual "Augment grammar test" expected result
  where
    sortGrammar (Grammar s t nt ps) = Grammar s (List.sort t) (List.sort nt) (List.sort ps)
    augmented_grammar = augmentGrammar grammar
    expected = sortGrammar augmentedGrammar
    result = sortGrammar augmented_grammar

tests =
  TestLabel "LLP(q, k) tests" $
    TestList
      [ augmentGrammarTestCase
      ]