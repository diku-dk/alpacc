module LL (tests) where

import qualified Data.List as List
import qualified Data.Set as Set
import ParallelParser.Grammar
import ParallelParser.LL
import ParallelParser.LLP
import Test.HUnit
import Debug.Trace (traceShow)
import Text.ParserCombinators.ReadP (string)
import Data.String.Interpolate (i)
import Data.Sequence (Seq (..), (<|), (><), (|>))
import qualified Data.Sequence as Seq hiding (Seq (..), (<|), (><), (|>))
import Data.Maybe

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

llkParseTestCase k = TestCase $ assertEqual [i|LL(#{k}) parsing test|] expected result
  where
    llkParse' = llParse k extendedGrammar
    input = List.singleton <$> "aabbbcc$"
    result = llkParse' (input, [Nonterminal $ start extendedGrammar], [])
    expected = Just ([], [], [0, 2, 2, 1, 4, 4, 4, 3])

llkParseTestCases = [llkParseTestCase k | k <- [1..10]]

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

firstkTestCases = [firstkTestCase k | k <- [1..20]]

followkTestCase k = TestCase $ assertEqual [i|Follow k=#{k} set test|] expected result
  where
    followsTuples string = (naiveFollow k extendedGrammar string, follow k extendedGrammar string)
    nonterminals' = nonterminals extendedGrammar
    (expected, result) = unzip $ followsTuples <$> nonterminals'

followkTestCases = [followkTestCase k | k <- [1..7]]

deriveNLengths n grammar = 
  Set.fromList
    . bfs
    . Seq.singleton
    . List.singleton
    . Nonterminal
    $ start grammar
  where
    unpackT (Terminal t) = t
    leftDerivations' = leftDerivations grammar
    bfs Empty = []
    bfs (top :<| queue)
      | length top > n = bfs queue
      | all isTerminal top = (unpackT <$> top) : bfs (queue >< leftDerivations' top)
      | otherwise = bfs (queue >< leftDerivations' top)

exntendedGrammarDerivations10 = deriveNLengths 20 extendedGrammar

canParseDerivedTestCase k = TestCase $ assertEqual text expected result
  where
    text = [i|Can parse derived strings og length 10 with LL(#{k}) parser|]
    expected = True
    result = all isJust $ llkParse' `Set.map` exntendedGrammarDerivations10 
    start' = Nonterminal $ start extendedGrammar
    llkParse' a = llParse k extendedGrammar (a, [start'], [])

canParseDerivedTestCases = [canParseDerivedTestCase k | k <- [1..10]]

tests =
  TestLabel "LL(k) tests" $
    TestList $
      [ nullableTestCase,
        firstSmallTestCase,
        followSmallTestCase,
        followLargeTestCase,
        ll1ParseFailTestCase
      ] ++ firstkTestCases
        ++ followkTestCases
        ++ canParseDerivedTestCases
        ++ llkParseTestCases
