module LL (tests) where

import qualified Data.List as List
import qualified Data.Set as Set
import ParallelParser.Grammar
import ParallelParser.LL
import Test.HUnit
import Debug.Trace (traceShow)
import Text.ParserCombinators.ReadP (string)

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
        ]
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
        ]
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
        ]
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
        ]
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

followSmallTestCase = TestCase $ assertEqual "Small Follow(1) test" expected result
  where
    follow' = follow 1 followExtendedGrammar
    result = follow' <$> nonterminals followExtendedGrammar
    expected =
      [ Set.fromList [["$"]],
        Set.fromList [[], ["$"], ["c"], ["b"]],
        Set.fromList [["$"], ["c"]]
      ]

followLargeTestCase = TestCase $ assertEqual "Large Follow(1) test" expected result
  where
    follow' = follow 1 bookGrammar
    result = follow' <$> nonterminals bookGrammar
    expected =
      [ Set.fromList [["$"]],
        Set.fromList [["$"]],
        Set.fromList [[], ["a"], ["b"], ["$"]],
        Set.fromList [[], ["a"], ["b"], ["$"]],
        Set.fromList [[], ["a"], ["b"], ["$"]]
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

first1TestCase = TestCase $ assertEqual "First k=1 set test" expected real
  where
    firstsTuples string = (naiveFirst 1 grammar string, first 1 grammar string)
    strings = symbols <$> productions grammar
    (expected, real) = unzip $ firstsTuples <$> strings 

first2TestCase = TestCase $ assertEqual "First k=2 set test" expected real
  where
    firstsTuples string = (naiveFirst 2 grammar string, first 2 grammar string)
    strings = symbols <$> productions grammar
    (expected, real) = unzip $ firstsTuples <$> strings 

first3TestCase = TestCase $ assertEqual "First k=3 set test" expected real
  where
    firstsTuples string = (naiveFirst 3 grammar string, first 3 grammar string)
    strings = symbols <$> productions grammar
    (expected, real) = unzip $ firstsTuples <$> strings 

first4TestCase = TestCase $ assertEqual "First k=4 set test" expected real
  where
    firstsTuples string = (naiveFirst 4 grammar string, first 4 grammar string)
    strings = symbols <$> productions grammar
    (expected, real) = unzip $ firstsTuples <$> strings 

tests =
  TestLabel "LL(k) tests" $
    TestList
      [ nullableTestCase,
        firstSmallTestCase,
        followSmallTestCase,
        followLargeTestCase,
        ll1ParseTestCase,
        ll1ParseFailTestCase,
        first1TestCase,
        first2TestCase,
        first3TestCase,
        first4TestCase
      ]
