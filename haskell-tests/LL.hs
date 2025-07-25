module LL (tests) where

import Alpacc.Grammar
import Alpacc.LL
import Alpacc.LLP hiding (LlpContext (..))
import Data.List qualified as List
import Data.Maybe
import Data.Sequence (Seq (..), (<|), (><), (|>))
import Data.Sequence qualified as Seq hiding (Seq (..), (<|), (><), (|>))
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase, (@?=))
import Text.ParserCombinators.ReadP (string)

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

firstSmallTestCase = testCase "Small First(1) test" $ expected @?= result
  where
    first' = first 1 grammar
    result = first' . symbols <$> productions grammar
    expected =
      [ Set.fromList [[], ["b"]],
        Set.fromList [["a"]],
        Set.fromList [[]],
        Set.fromList [["b"]]
      ]

firstLargeTestCase = testCase "Large First(1) test" $ result @?= expected
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

followSmallTestCase = testCase "Small Follow k=1 test" $ expected @?= result
  where
    follow' = follow 1 followExtendedGrammar
    result = follow' <$> nonterminals followExtendedGrammar
    expected =
      [ Set.fromList [[]],
        Set.fromList [["$"], ["c"], ["b"]],
        Set.fromList [["$"], ["c"]]
      ]

followLargeTestCase = testCase "Large Follow k=1 test" $ expected @?= result
  where
    follow' = follow 1 bookGrammar
    result = follow' <$> nonterminals bookGrammar
    expected =
      [ Set.fromList [[]],
        Set.fromList [["$"]],
        Set.fromList [["a"], ["b"], ["$"]],
        Set.fromList [["a"], ["b"], ["$"]],
        Set.fromList [["a"], ["b"], ["$"]]
      ]

llkParseTestCase k = testCase [i|LL(#{k}) parsing test|] $ expected @?= result
  where
    llkParse' = llParse k extendedGrammar
    input = List.singleton <$> "aabbbcc$"
    result = llkParse' (input, [Nonterminal $ start extendedGrammar], [])
    expected = Just ([], [], [0, 2, 2, 1, 4, 4, 4, 3])

llkParseTestCases = [llkParseTestCase k | k <- [1 .. 10]]

ll1ParseFailTestCase = testCase "LL(1) parsing test" $ expected @?= result
  where
    llkParse' = llParse 1 extendedGrammar
    input = List.singleton <$> "ab$"
    result = llkParse' (input, [Nonterminal $ start extendedGrammar], [])
    expected = Nothing

firstkTestCase :: (Show a, Show t, Ord a, Ord t) => Grammar a t -> Int -> TestTree
firstkTestCase grammar' k = testCase [i|First k=#{k} set test|] $ expected @?= result
  where
    firstsTuples string = (naiveFirst k grammar' string, first k grammar' string)
    strings = List.singleton . Nonterminal <$> nonterminals grammar'
    (expected, result) = unzip $ firstsTuples <$> strings

firstkTestCases grammar' m = [firstkTestCase grammar' k | k <- [1 .. m]]

followkTestCase grammar' k = testCase [i|Follow k=#{k} set test|] $ expected @?= result
  where
    followsTuples string = (naiveFollow k grammar' string, follow k grammar' string)
    nonterminals' = nonterminals grammar'
    (expected, result) = unzip $ followsTuples <$> nonterminals'

followkTestCases grammar' m = [followkTestCase grammar' k | k <- [1 .. m]]

firstMemokTestCase grammar' k = testCase [i|First k=#{k} set test|] $ expected @?= result
  where
    ctx = initFirstMemoizedContext k grammar'
    firstsTuples string = (naiveFirst k grammar' string, fst $ firstMemoized ctx string)
    strings = symbols <$> productions grammar'
    (expected, result) = unzip $ firstsTuples <$> strings

firstMemokTestCases grammar' m = [firstMemokTestCase grammar' k | k <- [1 .. m]]

extendedGrammarDerivations20 = derivableNLengths 20 extendedGrammar

extendedGrammarNotDerivations3 = nonderivableNLengths 3 extendedGrammar

canParseDerivedTestCase k = testCase text $ expected @?= result
  where
    text = [i|Can parse derived strings of length 20 with LL(#{k}) parser|]
    expected = True
    result = all isJust $ llkParse' `Set.map` extendedGrammarDerivations20
    start' = Nonterminal $ start extendedGrammar
    llkParse' a = llParse k extendedGrammar (a, [start'], [])

canNotParseNonderivableTestCase k = testCase text $ expected @?= result
  where
    text = [i|Can not parse nonderivable strings of length 3 with LL(#{k}) parser|]
    expected = True
    result = all isNothing $ llkParse' `Set.map` extendedGrammarNotDerivations3
    start' = Nonterminal $ start extendedGrammar
    llkParse' a = llParse k extendedGrammar (a, [start'], [])

canParseDerivedTestCases = [canParseDerivedTestCase k | k <- [1 .. 5]]

canNotParseNonderivableTestCases = [canNotParseNonderivableTestCase k | k <- [1 .. 5]]

tests =
  testGroup "LL(k) tests" $
    [ firstSmallTestCase,
      followSmallTestCase,
      followLargeTestCase,
      ll1ParseFailTestCase
    ]
      ++ firstkTestCases followExtendedGrammar 3
      ++ followkTestCases followExtendedGrammar 3
      ++ firstMemokTestCases followExtendedGrammar 3
      ++ firstkTestCases bookGrammar 3
      ++ followkTestCases bookGrammar 3
      ++ firstMemokTestCases bookGrammar 3
      ++ canParseDerivedTestCases
      ++ llkParseTestCases
      ++ canNotParseNonderivableTestCases
