module LLP (tests) where

import Alpacc.Grammar
import Alpacc.LL
import Alpacc.LLP
import Control.Monad.State
import Data.Either
import Data.Foldable (Foldable (toList))
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase, (@?=))

grammar :: Grammar String String
grammar =
  Grammar
    { start = "E",
      terminals = ["[", "]", "+", "a"],
      nonterminals = ["E", "E'", "T"],
      productions =
        [ Production "E" [Nonterminal "T", Nonterminal "E'"],
          Production "E'" [Terminal "+", Nonterminal "T", Nonterminal "E'"],
          Production "E'" [],
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
          Production (AugmentedNonterminal "E'") [],
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

augT = AugmentedTerminal

augNT = AugmentedNonterminal

augT' = Terminal . AugmentedTerminal

augNT' = Nonterminal . AugmentedNonterminal

leftTurnstile' = Terminal LeftTurnstile

rightTurnstile' = Terminal RightTurnstile

pslsTable =
  Map.fromList
    [ (([augT "+"], [augT "["]), Set.fromList [[augNT' "T"]]),
      (([augT "+"], [augT "a"]), Set.fromList [[augNT' "T"]]),
      (([augT "["], [augT "["]), Set.fromList [[augNT' "E"]]),
      (([augT "["], [augT "a"]), Set.fromList [[augNT' "E"]]),
      (([augT "]"], [augT "+"]), Set.fromList [[augNT' "E'"]]),
      (([augT "]"], [augT "]"]), Set.fromList [[augNT' "E'", augT' "]"]]),
      (([augT "]"], [LeftTurnstile]), Set.fromList [[augNT' "E'", leftTurnstile']]),
      (([augT "a"], [augT "+"]), Set.fromList [[augNT' "E'"]]),
      (([augT "a"], [augT "]"]), Set.fromList [[augNT' "E'", augT' "]"]]),
      (([augT "a"], [LeftTurnstile]), Set.fromList [[augNT' "E'", leftTurnstile']]),
      (([RightTurnstile], [augT "["]), Set.fromList [[augNT' "E"]]),
      (([RightTurnstile], [augT "a"]), Set.fromList [[augNT' "E"]])
    ]

collection :: Set.Set (Set.Set (Item (AugmentedNonterminal String) (AugmentedTerminal String)))
collection =
  Set.fromList
    [ Set.fromList -- = #
        [ Item -- 1. Item
            { dotProduction = DotProduction Start [rightTurnstile', augNT' "E", leftTurnstile'] [],
              suffix = [LeftTurnstile],
              prefix = [],
              llpConfig = ([], [], [])
            }
        ],
      Set.fromList -- = ⊣
        [ Item -- 1. Item
            { dotProduction = DotProduction Start [rightTurnstile', augNT' "E"] [leftTurnstile'],
              suffix = [augT "a"],
              prefix = [LeftTurnstile],
              llpConfig = ([leftTurnstile'], [], [])
            },
          Item
            { dotProduction = DotProduction Start [rightTurnstile', augNT' "E"] [leftTurnstile'],
              suffix = [augT "]"],
              prefix = [LeftTurnstile],
              llpConfig = ([leftTurnstile'], [], [])
            },
          Item -- 2. Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T", augNT' "E'"] [],
              suffix = [augT "a"],
              prefix = [LeftTurnstile],
              llpConfig = ([leftTurnstile'], [], [])
            },
          Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T", augNT' "E'"] [],
              suffix = [augT "]"],
              prefix = [LeftTurnstile],
              llpConfig = ([leftTurnstile'], [], [])
            },
          Item -- 3. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T", augNT' "E'"] [],
              suffix = [augT "a"],
              prefix = [LeftTurnstile],
              llpConfig = ([leftTurnstile'], [], [])
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T", augNT' "E'"] [],
              suffix = [augT "]"],
              prefix = [LeftTurnstile],
              llpConfig = ([leftTurnstile'], [], [])
            },
          Item -- 4. Item
            { dotProduction = DotProduction (augNT "E'") [] [],
              suffix = [augT "a"],
              prefix = [LeftTurnstile],
              llpConfig = ([leftTurnstile'], [], [])
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [] [],
              suffix = [augT "]"],
              prefix = [LeftTurnstile],
              llpConfig = ([leftTurnstile'], [], [])
            }
        ],
      Set.fromList -- = E1
        [ Item -- 1. Item
            { dotProduction = DotProduction Start [rightTurnstile'] [augNT' "E", leftTurnstile'],
              suffix = [RightTurnstile],
              prefix = [augT "a"],
              llpConfig = ([augNT' "E"], [augNT' "E'"], [1, 4])
            },
          Item
            { dotProduction = DotProduction Start [rightTurnstile'] [augNT' "E", leftTurnstile'],
              suffix = [RightTurnstile],
              prefix = [augT "["],
              llpConfig = ([augNT' "E"], [augNT' "E", augT' "]", augNT' "E'"], [1, 5])
            }
        ],
      Set.fromList -- = E'1
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = [augT "a"],
              prefix = [LeftTurnstile],
              llpConfig = ([augNT' "E'", leftTurnstile'], [], [3])
            },
          Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = [augT "]"],
              prefix = [LeftTurnstile],
              llpConfig = ([augNT' "E'", leftTurnstile'], [], [3])
            },
          Item -- 2. Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = [augT "a"],
              prefix = [augT "+"],
              llpConfig = ([augNT' "E'"], [augNT' "T", augNT' "E'"], [2])
            },
          Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = [augT "]"],
              prefix = [augT "+"],
              llpConfig = ([augNT' "E'"], [augNT' "T", augNT' "E'"], [2])
            },
          Item -- 3. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = [augT "a"],
              prefix = [LeftTurnstile],
              llpConfig = ([augNT' "E'", leftTurnstile'], [], [3])
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = [augT "]"],
              prefix = [LeftTurnstile],
              llpConfig = ([augNT' "E'", leftTurnstile'], [], [3])
            },
          Item -- 4. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = [augT "a"],
              prefix = [augT "+"],
              llpConfig = ([augNT' "E'"], [augNT' "T", augNT' "E'"], [2])
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = [augT "]"],
              prefix = [augT "+"],
              llpConfig = ([augNT' "E'"], [augNT' "T", augNT' "E'"], [2])
            },
          Item -- 5. Item
            { dotProduction = DotProduction (augNT "T") [augT' "a"] [],
              suffix = [augT "a"],
              prefix = [LeftTurnstile],
              llpConfig = ([augNT' "E'", leftTurnstile'], [], [3])
            },
          Item -- 6. Item
            { dotProduction = DotProduction (augNT "T") [augT' "a"] [],
              suffix = [augT "a"],
              prefix = [augT "+"],
              llpConfig = ([augNT' "E'"], [augNT' "T", augNT' "E'"], [2])
            },
          Item -- 5. Item
            { dotProduction = DotProduction (augNT "T") [augT' "[", augNT' "E", augT' "]"] [],
              suffix = [augT "]"],
              prefix = [LeftTurnstile],
              llpConfig = ([augNT' "E'", leftTurnstile'], [], [3])
            },
          Item -- 6. Item
            { dotProduction = DotProduction (augNT "T") [augT' "[", augNT' "E", augT' "]"] [],
              suffix = [augT "]"],
              prefix = [augT "+"],
              llpConfig = ([augNT' "E'"], [augNT' "T", augNT' "E'"], [2])
            }
        ],
      Set.fromList -- = ⊢
        [ Item -- 1. Item
            { dotProduction = DotProduction Start [] [rightTurnstile', augNT' "E", leftTurnstile'],
              suffix = [],
              prefix = [RightTurnstile],
              llpConfig = ([rightTurnstile'], [], [])
            }
        ],
      Set.fromList -- = T
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "E") [] [augNT' "T", augNT' "E'"],
              suffix = [RightTurnstile],
              prefix = [augT "a"],
              llpConfig = ([augNT' "T"], [], [4])
            },
          Item
            { dotProduction = DotProduction (augNT "E") [] [augNT' "T", augNT' "E'"],
              suffix = [augT "["],
              prefix = [augT "a"],
              llpConfig = ([augNT' "T"], [], [4])
            },
          Item
            { dotProduction = DotProduction (augNT "E") [] [augNT' "T", augNT' "E'"],
              suffix = [RightTurnstile],
              prefix = [augT "["],
              llpConfig = ([augNT' "T"], [augNT' "E", augT' "]"], [5])
            },
          Item
            { dotProduction = DotProduction (augNT "E") [] [augNT' "T", augNT' "E'"],
              suffix = [augT "["],
              prefix = [augT "["],
              llpConfig = ([augNT' "T"], [augNT' "E", augT' "]"], [5])
            },
          Item -- 2. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+"] [augNT' "T", augNT' "E'"],
              suffix = [augT "+"],
              prefix = [augT "a"],
              llpConfig = ([augNT' "T"], [], [4])
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+"] [augNT' "T", augNT' "E'"],
              suffix = [augT "+"],
              prefix = [augT "["],
              llpConfig = ([augNT' "T"], [augNT' "E", augT' "]"], [5])
            }
        ],
      Set.fromList -- = a
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "T") [] [augT' "a"],
              suffix = [RightTurnstile],
              prefix = [augT "a"],
              llpConfig = ([augT' "a"], [], [])
            },
          Item
            { dotProduction = DotProduction (augNT "T") [] [augT' "a"],
              suffix = [augT "+"],
              prefix = [augT "a"],
              llpConfig = ([augT' "a"], [], [])
            },
          Item
            { dotProduction = DotProduction (augNT "T") [] [augT' "a"],
              suffix = [augT "["],
              prefix = [augT "a"],
              llpConfig = ([augT' "a"], [], [])
            }
        ],
      Set.fromList -- = ]
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "T") [augT' "[", augNT' "E"] [augT' "]"],
              suffix = [augT "a"],
              prefix = [augT "]"],
              llpConfig = ([augT' "]"], [], [])
            },
          Item
            { dotProduction = DotProduction (augNT "T") [augT' "[", augNT' "E"] [augT' "]"],
              suffix = [augT "]"],
              prefix = [augT "]"],
              llpConfig = ([augT' "]"], [], [])
            },
          Item -- 2. Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T", augNT' "E'"] [],
              suffix = [augT "a"],
              prefix = [augT "]"],
              llpConfig = ([augT' "]"], [], [])
            },
          Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T", augNT' "E'"] [],
              suffix = [augT "]"],
              prefix = [augT "]"],
              llpConfig = ([augT' "]"], [], [])
            },
          Item -- 3. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T", augNT' "E'"] [],
              suffix = [augT "a"],
              prefix = [augT "]"],
              llpConfig = ([augT' "]"], [], [])
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T", augNT' "E'"] [],
              suffix = [augT "]"],
              prefix = [augT "]"],
              llpConfig = ([augT' "]"], [], [])
            },
          Item -- 4. Item
            { dotProduction = DotProduction (augNT "E'") [] [],
              suffix = [augT "a"],
              prefix = [augT "]"],
              llpConfig = ([augT' "]"], [], [])
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [] [],
              suffix = [augT "]"],
              prefix = [augT "]"],
              llpConfig = ([augT' "]"], [], [])
            }
        ],
      Set.fromList -- = +
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "E'") [] [augT' "+", augNT' "T", augNT' "E'"],
              suffix = [augT "a"],
              prefix = [augT "+"],
              llpConfig = ([augT' "+"], [], [])
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [] [augT' "+", augNT' "T", augNT' "E'"],
              suffix = [augT "]"],
              prefix = [augT "+"],
              llpConfig = ([augT' "+"], [], [])
            }
        ],
      Set.fromList -- = E2
        [ Item -- 1. Item (I believe the E -> [.E] from the paper is an error so I use T -> [.E].)
            { dotProduction = DotProduction (augNT "T") [augT' "["] [augNT' "E", augT' "]"],
              suffix = [augT "["],
              prefix = [augT "a"],
              llpConfig = ([augNT' "E"], [augNT' "E'"], [1, 4])
            },
          Item
            { dotProduction = DotProduction (augNT "T") [augT' "["] [augNT' "E", augT' "]"],
              suffix = [augT "["],
              prefix = [augT "["],
              llpConfig = ([augNT' "E"], [augNT' "E", augT' "]", augNT' "E'"], [1, 5])
            }
        ],
      Set.fromList -- = E'2
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = [augT "a"],
              prefix = [augT "]"],
              llpConfig = ([augNT' "E'", augT' "]"], [], [3])
            },
          Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = [augT "]"],
              prefix = [augT "]"],
              llpConfig = ([augNT' "E'", augT' "]"], [], [3])
            },
          Item -- 2. Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = [augT "a"],
              prefix = [augT "+"],
              llpConfig = ([augNT' "E'"], [augNT' "T", augNT' "E'"], [2])
            },
          Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = [augT "]"],
              prefix = [augT "+"],
              llpConfig = ([augNT' "E'"], [augNT' "T", augNT' "E'"], [2])
            },
          Item -- 3. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = [augT "a"],
              prefix = [augT "]"],
              llpConfig = ([augNT' "E'", augT' "]"], [], [3])
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = [augT "]"],
              prefix = [augT "]"],
              llpConfig = ([augNT' "E'", augT' "]"], [], [3])
            },
          Item -- 4. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = [augT "a"],
              prefix = [augT "+"],
              llpConfig = ([augNT' "E'"], [augNT' "T", augNT' "E'"], [2])
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = [augT "]"],
              prefix = [augT "+"],
              llpConfig = ([augNT' "E'"], [augNT' "T", augNT' "E'"], [2])
            },
          Item -- 5. Item
            { dotProduction = DotProduction (augNT "T") [augT' "a"] [],
              suffix = [augT "a"],
              prefix = [augT "]"],
              llpConfig = ([augNT' "E'", augT' "]"], [], [3])
            },
          Item -- 6. Item
            { dotProduction = DotProduction (augNT "T") [augT' "a"] [],
              suffix = [augT "a"],
              prefix = [augT "+"],
              llpConfig = ([augNT' "E'"], [augNT' "T", augNT' "E'"], [2])
            },
          Item -- 7. Item
            { dotProduction = DotProduction (augNT "T") [augT' "[", augNT' "E", augT' "]"] [],
              suffix = [augT "]"],
              prefix = [augT "]"],
              llpConfig = ([augNT' "E'", augT' "]"], [], [3])
            },
          Item -- 8. Item
            { dotProduction = DotProduction (augNT "T") [augT' "[", augNT' "E", augT' "]"] [],
              suffix = [augT "]"],
              prefix = [augT "+"],
              llpConfig = ([augNT' "E'"], [augNT' "T", augNT' "E'"], [2])
            }
        ],
      Set.fromList -- = [
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "T") [] [augT' "[", augNT' "E", augT' "]"],
              suffix = [RightTurnstile],
              prefix = [augT "["],
              llpConfig = ([augT' "["], [], [])
            },
          Item
            { dotProduction = DotProduction (augNT "T") [] [augT' "[", augNT' "E", augT' "]"],
              suffix = [augT "+"],
              prefix = [augT "["],
              llpConfig = ([augT' "["], [], [])
            },
          Item
            { dotProduction = DotProduction (augNT "T") [] [augT' "[", augNT' "E", augT' "]"],
              suffix = [augT "["],
              prefix = [augT "["],
              llpConfig = ([augT' "["], [], [])
            }
        ]
    ]

augmentGrammarTestCase = testCase "Augment grammar test" $ expected @?= result
  where
    sortGrammar (Grammar s t nt ps) = Grammar s (List.sort t) (List.sort nt) (List.sort ps)
    augmented_grammar = augmentGrammar grammar
    expected = sortGrammar augmentedGrammar
    result = sortGrammar augmented_grammar

llpqkParsingTestCase parser q k = testCase [i|LLP(#{q}, #{k}) parse test|] $ expected @?= result
  where
    input = map List.singleton "a+[a+a]"
    result = parser input
    expected = Right [0, 0, 3, 1, 4, 0, 3, 1, 3, 2, 2]

llpParsers = [(llpParse q k grammar, q, k) | q <- [1 .. 3], k <- [1 .. 3]]

derivable10 = derivableNLengths 10 grammar

llpqkParsingDerivableTestCase parser q k =
  testCase [i|LLP(#{q}, #{k}) can parse derivables of length 10.|] $ expected @?= True
  where
    expected = all isRight $ parser <$> Set.toList derivable10

llpqkParsingDerivableTestCases = [llpqkParsingDerivableTestCase parser q k | (parser, k, q) <- llpParsers]

llpqkParsingTestCases = [llpqkParsingTestCase parser q k | (parser, k, q) <- llpParsers]

nonderivable3 = nonderivableNLengths 3 grammar

llpqkParsingNonderivableTestCase parser q k =
  testCase [i|LLP(#{q}, #{k}) fails on parsing nonderivables of length 3.|] $ expected @?= True
  where
    expected = all isLeft $ parser <$> Set.toList nonderivable3

llpqkParsingNonderivableTestCases = [llpqkParsingNonderivableTestCase parser q k | (parser, k, q) <- llpParsers]

tests =
  testGroup "LLP(q,k) tests" $
    [ augmentGrammarTestCase
    ]
      ++ llpqkParsingTestCases
      ++ llpqkParsingDerivableTestCases
      ++ llpqkParsingNonderivableTestCases
