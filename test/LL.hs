module LL (tests) where

import qualified Data.List as List
import qualified Data.Set as Set
import ParallelParser.Grammar
import ParallelParser.LL
import Test.HUnit
import Debug.Trace (traceShow)
import Text.ParserCombinators.ReadP (string)
import Data.String.Interpolate (i)

debug x = traceShow x x

grammar =
  Grammar
    { start = "T",
      terminals = ["a", "b", "c"],
      nonterminals = ["R", "T"],
      productions =
        [ Production "T" [Nonterminal "R"],
          Production "T" [Terminal "a", Nonterminal "T", Terminal "c"],
          Production "R" [],
          Production "R" [Terminal "b", Nonterminal "R"]
        ],
      leftPadding = Nothing,
      rightPadding = Nothing
    }

followExtendedGrammar =
  Grammar
    { start = "T'",
      terminals = ["$", "a", "b", "c"],
      nonterminals = ["T'", "R", "T"],
      productions =
        [ Production "T'" [Nonterminal "T", Terminal "$"],
          Production "T" [Nonterminal "R"],
          Production "T" [Terminal "a", Nonterminal "T", Terminal "c"],
          Production "R" [],
          Production "R" [Nonterminal "R", Terminal "b", Nonterminal "R"]
        ],
      leftPadding = Nothing,
      rightPadding = Just "$"
    }

extendedGrammar =
  Grammar
    { start = "T'",
      terminals = ["$", "a", "b", "c"],
      nonterminals = ["T'", "R", "T"],
      productions =
        [ Production "T'" [Nonterminal "T", Terminal "$"],
          Production "T" [Nonterminal "R"],
          Production "T" [Terminal "a", Nonterminal "T", Terminal "c"],
          Production "R" [],
          Production "R" [Terminal "b", Nonterminal "R"]
        ],
      leftPadding = Nothing,
      rightPadding = Just "$"
    }

bookGrammar =
  Grammar
    { start = "N'",
      terminals = ["$", "a", "b"],
      nonterminals = ["N'", "N", "A", "B", "C"],
      productions =
        [ Production "N'" [Nonterminal "N", Terminal "$"],
          Production "N" [Nonterminal "A", Nonterminal "B"],
          Production "N" [Nonterminal "B", Nonterminal "A"],
          Production "A" [Terminal "a"],
          Production "A" [Nonterminal "C", Nonterminal "A", Nonterminal "C"],
          Production "B" [Terminal "b"],
          Production "B" [Nonterminal "C", Nonterminal "B", Nonterminal "C"],
          Production "C" [Terminal "a"],
          Production "C" [Terminal "b"]
        ],
      leftPadding = Nothing,
      rightPadding = Just "$"
    }

nullableTestCase = TestCase $ assertEqual "Nullable test" expected result
  where
    nullable' = nullable grammar
    result = nullable' . symbols <$> productions grammar
    expected = [True, False, True, False]

firstSmallTestCase = TestCase $ assertEqual "Small First(1) test" expected result
  where
    first' = first 1 grammar
    result = first' . symbols <$> productions grammar
    expected =
      [ Set.fromList [[], ["b"]],
        Set.fromList [["a"]],
        Set.fromList [[]],
        Set.fromList [["b"]]
      ]

firstLargeTestCase = TestCase $ assertEqual "Large First(1) test" result expected
  where
    first' = first 1 grammar
    result = first' . symbols <$> productions grammar
    expected =
      [ Set.fromList [["a"], ["b"]],
        Set.fromList [["a"], ["b"]],
        Set.fromList [["a"], ["b"]],
        Set.fromList [["a"]],
        Set.fromList [["a"], ["b"]],
        Set.fromList [["b"]],
        Set.fromList [["a"], ["b"]],
        Set.fromList [["a"]],
        Set.fromList [["b"]]
      ]

followSmallTestCase = TestCase $ assertEqual "Small Follow k=1 test" expected result
  where
    follow' = follow 1 followExtendedGrammar
    result = follow' <$> nonterminals followExtendedGrammar
    expected =
      [ Set.fromList [],
        Set.fromList [["$"], ["c"], ["b"]],
        Set.fromList [["$"], ["c"]]
      ]

followLargeTestCase = TestCase $ assertEqual "Large Follow k=1 test" expected result
  where
    follow' = follow 1 bookGrammar
    result = follow' <$> nonterminals bookGrammar
    expected =
      [ Set.fromList [],
        Set.fromList [["$"]],
        Set.fromList [["a"], ["b"], ["$"]],
        Set.fromList [["a"], ["b"], ["$"]],
        Set.fromList [["a"], ["b"], ["$"]]
      ]

ll1ParseTestCase = TestCase $ assertEqual "LL(1) parsing test" expected result
  where
    llkParse' = llParse 1 extendedGrammar
    input = List.singleton <$> "aabbbcc$"
    result = llkParse' (input, [Nonterminal $ start extendedGrammar], [])
    expected = Just ([], [], [0, 2, 2, 1, 4, 4, 4, 3])

ll1ParseFailTestCase = TestCase $ assertEqual "LL(1) parsing test" expected result
  where
    llkParse' = llParse 1 extendedGrammar
    input = List.singleton <$> "ab$"
    result = llkParse' (input, [Nonterminal $ start extendedGrammar], [])
    expected = Nothing

firstkTestCase k = TestCase $ assertEqual [i|First k=#{k} set test|] expected result
  where
    firstsTuples string = (naiveFirst k extendedGrammar string, first k extendedGrammar string)
    strings = symbols <$> productions extendedGrammar
    (expected, result) = unzip $ firstsTuples <$> strings

firstkTestCases = [firstkTestCase k | k <- [1..3]]

followkTestCase k = TestCase $ assertEqual [i|Follow k=#{k} set test|] expected result
  where
    extended_grammar = extendGrammar k extendedGrammar
    followsTuples string = (naiveFollow k extended_grammar string, follow k extended_grammar string)
    nonterminals' = nonterminals extended_grammar
    (expected, result) = unzip $ followsTuples <$> nonterminals'

followkTestCases = [followkTestCase k | k <- [1..3]]

tests =
  TestLabel "LL(k) tests" $
    TestList $
      [ nullableTestCase,
        firstSmallTestCase,
        followSmallTestCase,
        followLargeTestCase,
        ll1ParseTestCase,
        ll1ParseFailTestCase
      ] ++ firstkTestCases
        ++ followkTestCases
