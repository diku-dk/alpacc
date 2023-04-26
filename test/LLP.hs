module LLP (tests) where

import Data.Foldable (Foldable (toList))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import ParallelParser.Grammar
import ParallelParser.LLP
import Test.HUnit
import Data.String.Interpolate (i)
import Debug.Trace (traceShow)

debug x = traceShow x x

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
              suffix = Set.singleton [LeftTurnstile],
              prefix = [],
              shortestPrefix = []
            }
        ],
      Set.fromList -- = ⊣
        [ Item -- 1. Item
            { dotProduction = DotProduction Start [rightTurnstile', augNT' "E"] [leftTurnstile'],
              suffix = Set.singleton  [augT "a"],
              prefix = [LeftTurnstile],
              shortestPrefix = [leftTurnstile']
            },
          Item
            { dotProduction = DotProduction Start [rightTurnstile', augNT' "E"] [leftTurnstile'],
              suffix = Set.singleton [augT "]"],
              prefix = [LeftTurnstile],
              shortestPrefix = [leftTurnstile']
            },
          Item -- 2. Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T", augNT' "E'"] [],
              suffix = Set.singleton [augT "a"],
              prefix = [LeftTurnstile],
              shortestPrefix = [leftTurnstile']
            },
          Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T", augNT' "E'"] [],
              suffix = Set.singleton [augT "]"],
              prefix = [LeftTurnstile],
              shortestPrefix = [leftTurnstile']
            },
          Item -- 3. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T", augNT' "E'"] [],
              suffix = Set.singleton [augT "a"],
              prefix = [LeftTurnstile],
              shortestPrefix = [leftTurnstile']
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T", augNT' "E'"] [],
              suffix = Set.singleton [augT "]"],
              prefix = [LeftTurnstile],
              shortestPrefix = [leftTurnstile']
            },
          Item -- 4. Item
            { dotProduction = DotProduction (augNT "E'") [] [],
              suffix = Set.singleton  [augT "a"],
              prefix = [LeftTurnstile],
              shortestPrefix = [leftTurnstile']
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [] [],
              suffix = Set.singleton [augT "]"],
              prefix = [LeftTurnstile],
              shortestPrefix = [leftTurnstile']
            }
        ],
      Set.fromList -- = E1
        [ Item -- 1. Item
            { dotProduction = DotProduction Start [rightTurnstile'] [augNT' "E", leftTurnstile'],
              suffix = Set.singleton [RightTurnstile],
              prefix = [augT "a"],
              shortestPrefix = [augNT' "E"]
            },
          Item
            { dotProduction = DotProduction Start [rightTurnstile'] [augNT' "E", leftTurnstile'],
              suffix = Set.singleton [RightTurnstile],
              prefix = [augT "["],
              shortestPrefix = [augNT' "E"]
            }
        ],
      Set.fromList -- = E'1
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = Set.singleton [augT "a"],
              prefix = [LeftTurnstile],
              shortestPrefix = [augNT' "E'", leftTurnstile']
            },
          Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = Set.singleton [augT "]"],
              prefix = [LeftTurnstile],
              shortestPrefix = [augNT' "E'", leftTurnstile']
            },
          Item -- 2. Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = Set.singleton [augT "a"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = Set.singleton [augT "]"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item -- 3. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = Set.singleton [augT "a"],
              prefix = [LeftTurnstile],
              shortestPrefix = [augNT' "E'", leftTurnstile']
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = Set.singleton [augT "]"],
              prefix = [LeftTurnstile],
              shortestPrefix = [augNT' "E'", leftTurnstile']
            },
          Item -- 4. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = Set.singleton [augT "a"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = Set.singleton [augT "]"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item -- 5. Item
            { dotProduction = DotProduction (augNT "T") [augT' "a"] [],
              suffix = Set.singleton [augT "a"],
              prefix = [LeftTurnstile],
              shortestPrefix = [augNT' "E'", leftTurnstile']
            },
          Item -- 6. Item
            { dotProduction = DotProduction (augNT "T") [augT' "a"] [],
              suffix = Set.singleton [augT "a"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item -- 5. Item
            { dotProduction = DotProduction (augNT "T") [augT' "[", augNT' "E", augT' "]"] [],
              suffix = Set.singleton [augT "]"],
              prefix = [LeftTurnstile],
              shortestPrefix = [augNT' "E'", leftTurnstile']
            },
          Item -- 6. Item
            { dotProduction = DotProduction (augNT "T") [augT' "[", augNT' "E", augT' "]"] [],
              suffix = Set.singleton [augT "]"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            }
        ],
      Set.fromList -- = ⊢
        [ Item -- 1. Item
            { dotProduction = DotProduction Start [] [rightTurnstile', augNT' "E", leftTurnstile'],
              suffix = Set.singleton [],
              prefix = [RightTurnstile],
              shortestPrefix = [rightTurnstile']
            }
        ],
      Set.fromList -- = T
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "E") [] [augNT' "T", augNT' "E'"],
              suffix = Set.singleton [RightTurnstile],
              prefix = [augT "a"],
              shortestPrefix = [augNT' "T"]
            },
          Item
            { dotProduction = DotProduction (augNT "E") [] [augNT' "T", augNT' "E'"],
              suffix = Set.singleton [augT "["],
              prefix = [augT "a"],
              shortestPrefix = [augNT' "T"]
            },
          Item
            { dotProduction = DotProduction (augNT "E") [] [augNT' "T", augNT' "E'"],
              suffix = Set.singleton [RightTurnstile],
              prefix = [augT "["],
              shortestPrefix = [augNT' "T"]
            },
          Item
            { dotProduction = DotProduction (augNT "E") [] [augNT' "T", augNT' "E'"],
              suffix = Set.singleton [augT "["],
              prefix = [augT "["],
              shortestPrefix = [augNT' "T"]
            },
          Item -- 2. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+"] [augNT' "T", augNT' "E'"],
              suffix = Set.singleton [augT "+"],
              prefix = [augT "a"],
              shortestPrefix = [augNT' "T"]
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+"] [augNT' "T", augNT' "E'"],
              suffix = Set.singleton [augT "+"],
              prefix = [augT "["],
              shortestPrefix = [augNT' "T"]
            }
        ],
      Set.fromList -- = a
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "T") [] [augT' "a"],
              suffix = Set.singleton [RightTurnstile],
              prefix = [augT "a"],
              shortestPrefix = [augT' "a"]
            },
          Item
            { dotProduction = DotProduction (augNT "T") [] [augT' "a"],
              suffix = Set.singleton [augT "+"],
              prefix = [augT "a"],
              shortestPrefix = [augT' "a"]
            },
          Item
            { dotProduction = DotProduction (augNT "T") [] [augT' "a"],
              suffix = Set.singleton [augT "["],
              prefix = [augT "a"],
              shortestPrefix = [augT' "a"]
            }
        ],
      Set.fromList -- = ]
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "T") [augT' "[", augNT' "E"] [augT' "]"],
              suffix = Set.singleton [augT "a"],
              prefix = [augT "]"],
              shortestPrefix = [augT' "]"]
            },
          Item
            { dotProduction = DotProduction (augNT "T") [augT' "[", augNT' "E"] [augT' "]"],
              suffix = Set.singleton [augT "]"],
              prefix = [augT "]"],
              shortestPrefix = [augT' "]"]
            },
          Item -- 2. Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T", augNT' "E'"] [],
              suffix = Set.singleton [augT "a"],
              prefix = [augT "]"],
              shortestPrefix = [augT' "]"]
            },
          Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T", augNT' "E'"] [],
              suffix = Set.singleton [augT "]"],
              prefix = [augT "]"],
              shortestPrefix = [augT' "]"]
            },
          Item -- 3. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T", augNT' "E'"] [],
              suffix = Set.singleton [augT "a"],
              prefix = [augT "]"],
              shortestPrefix = [augT' "]"]
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T", augNT' "E'"] [],
              suffix = Set.singleton [augT "]"],
              prefix = [augT "]"],
              shortestPrefix = [augT' "]"]
            },
          Item -- 4. Item
            { dotProduction = DotProduction (augNT "E'") [] [],
              suffix = Set.singleton [augT "a"],
              prefix = [augT "]"],
              shortestPrefix = [augT' "]"]
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [] [],
              suffix = Set.singleton [augT "]"],
              prefix = [augT "]"],
              shortestPrefix = [augT' "]"]
            }
        ],
      Set.fromList -- = +
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "E'") [] [augT' "+", augNT' "T", augNT' "E'"],
              suffix = Set.singleton [augT "a"],
              prefix = [augT "+"],
              shortestPrefix = [augT' "+"]
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [] [augT' "+", augNT' "T", augNT' "E'"],
              suffix = Set.singleton [augT "]"],
              prefix = [augT "+"],
              shortestPrefix = [augT' "+"]
            }
        ],
      Set.fromList -- = E2
        [ Item -- 1. Item (I believe the E -> [.E] from the paper is an error so I use T -> [.E].)
            { dotProduction = DotProduction (augNT "T") [augT' "["] [augNT' "E", augT' "]"],
              suffix = Set.singleton [augT "["],
              prefix = [augT "a"],
              shortestPrefix = [augNT' "E"]
            },
          Item
            { dotProduction = DotProduction (augNT "T") [augT' "["] [augNT' "E", augT' "]"],
              suffix = Set.singleton [augT "["],
              prefix = [augT "["],
              shortestPrefix = [augNT' "E"]
            }
        ],
      Set.fromList -- = E'2
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = Set.singleton [augT "a"],
              prefix = [augT "]"],
              shortestPrefix = [augNT' "E'", augT' "]"]
            },
          Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = Set.singleton [augT "]"],
              prefix = [augT "]"],
              shortestPrefix = [augNT' "E'", augT' "]"]
            },
          Item -- 2. Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = Set.singleton [augT "a"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = Set.singleton [augT "]"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item -- 3. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = Set.singleton [augT "a"],
              prefix = [augT "]"],
              shortestPrefix = [augNT' "E'", augT' "]"]
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = Set.singleton [augT "]"],
              prefix = [augT "]"],
              shortestPrefix = [augNT' "E'", augT' "]"]
            },
          Item -- 4. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = Set.singleton [augT "a"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = Set.singleton [augT "]"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item -- 5. Item
            { dotProduction = DotProduction (augNT "T") [augT' "a"] [],
              suffix = Set.singleton [augT "a"],
              prefix = [augT "]"],
              shortestPrefix = [augNT' "E'", augT' "]"]
            },
          Item -- 6. Item
            { dotProduction = DotProduction (augNT "T") [augT' "a"] [],
              suffix = Set.singleton [augT "a"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item -- 7. Item
            { dotProduction = DotProduction (augNT "T") [augT' "[", augNT' "E", augT' "]"] [],
              suffix = Set.singleton [augT "]"],
              prefix = [augT "]"],
              shortestPrefix = [augNT' "E'", augT' "]"]
            },
          Item -- 8. Item
            { dotProduction = DotProduction (augNT "T") [augT' "[", augNT' "E", augT' "]"] [],
              suffix = Set.singleton [augT "]"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            }
        ],
      Set.fromList -- = [
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "T") [] [augT' "[", augNT' "E", augT' "]"],
              suffix = Set.singleton [RightTurnstile],
              prefix = [augT "["],
              shortestPrefix = [augT' "["]
            },
          Item
            { dotProduction = DotProduction (augNT "T") [] [augT' "[", augNT' "E", augT' "]"],
              suffix = Set.singleton [augT "+"],
              prefix = [augT "["],
              shortestPrefix = [augT' "["]
            },
          Item
            { dotProduction = DotProduction (augNT "T") [] [augT' "[", augNT' "E", augT' "]"],
              suffix = Set.singleton [augT "["],
              prefix = [augT "["],
              shortestPrefix = [augT' "["]
            }
        ]
    ]

augmentGrammarTestCase = TestCase $ assertEqual "Augment grammar test" expected result
  where
    sortGrammar (Grammar s t nt ps) = Grammar s (List.sort t) (List.sort nt) (List.sort ps) 
    augmented_grammar = augmentGrammar 1 1 grammar
    expected = sortGrammar augmentedGrammar
    result = sortGrammar augmented_grammar

collectionTestCase = TestCase $ assertEqual "LLP collection test" expected result
  where
    expected = Set.empty
    computed_collection = Set.map (Set.unions . Set.map unpackSets) $ llpCollection 1 1 augmentedGrammar
    result = Set.map debug $ Set.difference computed_collection collection
    unpackSets (Item dp s p sp) = Set.fromList [Item dp (Set.singleton s') p sp | s' <- toList s]

pslsTestCase = TestCase $ assertEqual "PSLS table test" expected result
  where
    expected = pslsTable
    collection' = llpCollection 1 1 augmentedGrammar
    result = psls collection'

llpqkParsingTestCase q k = TestCase $ assertEqual [i|LLP(#{q}, #{k}) parse test|] expected result
  where
    input = map List.singleton "a+[a+a]"
    result = parser q k
    expected = [0, 1, 4, 2, 5, 1, 4, 2, 4, 3, 3]
    parser q k = llpParse q k grammar input

llpqkParsingTestCases = [llpqkParsingTestCase q k | q <- [1..5], k <- [1..5]]

tests = 
  TestLabel "LLP(q, k) tests" $
    TestList $
      [ augmentGrammarTestCase,
        pslsTestCase,
        collectionTestCase
      ] ++ llpqkParsingTestCases
