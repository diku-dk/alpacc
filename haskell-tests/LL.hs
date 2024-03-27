module LL (tests) where

import qualified Data.List as List
import qualified Data.Set as Set
import Alpacc.Grammar
import Alpacc.LL
import Alpacc.LLP hiding ( LlpContext(..) )
import Test.HUnit
import Text.ParserCombinators.ReadP (string)
import Data.String.Interpolate (i)
import Data.Sequence (Seq (..), (<|), (><), (|>))
import qualified Data.Sequence as Seq hiding (Seq (..), (<|), (><), (|>))
import Data.Maybe

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
      [ Set.fromList [[]],
        Set.fromList [["$"], ["c"], ["b"]],
        Set.fromList [["$"], ["c"]]
      ]

followLargeTestCase = TestCase $ assertEqual "Large Follow k=1 test" expected result
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

firstkTestCase :: (Show a, Show t, Ord a, Ord t) => Grammar a t -> Int -> Test
firstkTestCase grammar' k = TestCase $ assertEqual [i|First k=#{k} set test|] expected result
  where
    firstsTuples string = (naiveFirst k grammar' string, first k grammar' string)
    strings = List.singleton . Nonterminal <$> nonterminals grammar'
    (expected, result) = unzip $ firstsTuples <$> strings

firstkTestCases grammar' m = [firstkTestCase grammar' k | k <- [1..m]]

followkTestCase grammar' k = TestCase $ assertEqual [i|Follow k=#{k} set test|] expected result
  where
    followsTuples string = (naiveFollow k grammar' string, follow k grammar' string)
    nonterminals' = nonterminals grammar'
    (expected, result) = unzip $ followsTuples <$> nonterminals'

followkTestCases grammar' m = [followkTestCase grammar' k | k <- [1..m]]

firstMemokTestCase grammar' k = TestCase $ assertEqual [i|First k=#{k} set test|] expected result
  where
    ctx = initFirstMemoizedContext k grammar'
    firstsTuples string = (naiveFirst k grammar' string, fst $ firstMemoized ctx string)
    strings = symbols <$> productions grammar'
    (expected, result) = unzip $ firstsTuples <$> strings

firstMemokTestCases grammar' m = [firstMemokTestCase grammar' k | k <- [1..m]]

extendedGrammarDerivations20 = derivableNLengths 20 extendedGrammar

extendedGrammarNotDerivations3 = nonderivableNLengths 3 extendedGrammar

canParseDerivedTestCase k = TestCase $ assertEqual text expected result
  where
    text = [i|Can parse derived strings of length 20 with LL(#{k}) parser|]
    expected = True
    result = all isJust $ llkParse' `Set.map` extendedGrammarDerivations20 
    start' = Nonterminal $ start extendedGrammar
    llkParse' a = llParse k extendedGrammar (a, [start'], [])

canNotParseNonderivableTestCase k = TestCase $ assertEqual text expected result
  where
    text = [i|Can not parse nonderivable strings of length 3 with LL(#{k}) parser|]
    expected = True
    result = all isNothing $ llkParse' `Set.map` extendedGrammarNotDerivations3 
    start' = Nonterminal $ start extendedGrammar
    llkParse' a = llParse k extendedGrammar (a, [start'], [])

canParseDerivedTestCases = [canParseDerivedTestCase k | k <- [1..5]]

canNotParseNonderivableTestCases = [canNotParseNonderivableTestCase k | k <- [1..5]]

tests =
  TestLabel "LL(k) tests" $
    TestList $
      [ nullableTestCase,
        firstSmallTestCase,
        followSmallTestCase,
        followLargeTestCase,
        ll1ParseFailTestCase
      ] ++ firstkTestCases followExtendedGrammar 4
        ++ followkTestCases followExtendedGrammar 4
        ++ firstMemokTestCases followExtendedGrammar 4
        ++ firstkTestCases bookGrammar 4
        ++ followkTestCases bookGrammar 4
        ++ firstMemokTestCases bookGrammar 4
        ++ canParseDerivedTestCases
        ++ llkParseTestCases
        ++ canNotParseNonderivableTestCases
