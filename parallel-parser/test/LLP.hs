module LLP (tests) where

import Data.Composition
import Data.Foldable (Foldable (toList))
import Data.Function
import qualified Data.IntMap as S
import qualified Data.List as List
import qualified Data.Map as Map
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
    [ (([augT "+"], [augT "["]), [augNT' "T"]),
      (([augT "+"], [augT "a"]), [augNT' "T"]),
      (([augT "["], [augT "["]), [augNT' "E"]),
      (([augT "["], [augT "a"]), [augNT' "E"]),
      (([augT "]"], [augT "+"]), [augNT' "E'"]),
      (([augT "]"], [augT "]"]), [augNT' "E'", augT' "]"]),
      (([augT "]"], [LeftTurnstile]), [augNT' "E'", leftTurnstile']),
      (([augT "a"], [augT "+"]), [augNT' "E'"]),
      (([augT "a"], [augT "]"]), [augNT' "E'", augT' "]"]),
      (([augT "a"], [LeftTurnstile]), [augNT' "E'", leftTurnstile']),
      (([RightTurnstile], [augT "["]), [augNT' "E"]),
      (([RightTurnstile], [augT "a"]), [augNT' "E"])
    ]

collection :: Set.Set (Set.Set (Item (AugmentedNonterminal String) (AugmentedTerminal String)))
collection =
  Set.fromList
    [ Set.fromList -- = #
        [ Item -- 1. Item
            { dotProduction = DotProduction Start [rightTurnstile', augNT' "E", leftTurnstile'] [],
              suffix = [LeftTurnstile],
              prefix = [],
              shortestPrefix = []
            }
        ],
      Set.fromList -- = ⊣
        [ Item -- 1. Item
            { dotProduction = DotProduction Start [rightTurnstile', augNT' "E"] [leftTurnstile'],
              suffix = [augT "a"],
              prefix = [LeftTurnstile],
              shortestPrefix = [leftTurnstile']
            },
          Item
            { dotProduction = DotProduction Start [rightTurnstile', augNT' "E"] [leftTurnstile'],
              suffix = [augT "]"],
              prefix = [LeftTurnstile],
              shortestPrefix = [leftTurnstile']
            },
          Item -- 2. Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T", augNT' "E'"] [],
              suffix = [augT "a"],
              prefix = [LeftTurnstile],
              shortestPrefix = [leftTurnstile']
            },
          Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T", augNT' "E'"] [],
              suffix = [augT "]"],
              prefix = [LeftTurnstile],
              shortestPrefix = [leftTurnstile']
            },
          Item -- 3. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T", augNT' "E'"] [],
              suffix = [augT "a"],
              prefix = [LeftTurnstile],
              shortestPrefix = [leftTurnstile']
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T", augNT' "E'"] [],
              suffix = [augT "]"],
              prefix = [LeftTurnstile],
              shortestPrefix = [leftTurnstile']
            },
          Item -- 4. Item
            { dotProduction = DotProduction (augNT "E'") [] [],
              suffix = [augT "a"],
              prefix = [LeftTurnstile],
              shortestPrefix = [leftTurnstile']
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [] [],
              suffix = [augT "]"],
              prefix = [LeftTurnstile],
              shortestPrefix = [leftTurnstile']
            }
        ],
      Set.fromList -- = E1
        [ Item -- 1. Item
            { dotProduction = DotProduction Start [rightTurnstile'] [augNT' "E", leftTurnstile'],
              suffix = [RightTurnstile],
              prefix = [augT "a"],
              shortestPrefix = [augNT' "E"]
            },
          Item
            { dotProduction = DotProduction Start [rightTurnstile'] [augNT' "E", leftTurnstile'],
              suffix = [RightTurnstile],
              prefix = [augT "["],
              shortestPrefix = [augNT' "E"]
            }
        ],
      Set.fromList -- = E'1
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = [augT "a"],
              prefix = [LeftTurnstile],
              shortestPrefix = [augNT' "E'", leftTurnstile']
            },
          Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = [augT "]"],
              prefix = [LeftTurnstile],
              shortestPrefix = [augNT' "E'", leftTurnstile']
            },
          Item -- 2. Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = [augT "a"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = [augT "]"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item -- 3. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = [augT "a"],
              prefix = [LeftTurnstile],
              shortestPrefix = [augNT' "E'", leftTurnstile']
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = [augT "]"],
              prefix = [LeftTurnstile],
              shortestPrefix = [augNT' "E'", leftTurnstile']
            },
          Item -- 4. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = [augT "a"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = [augT "]"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item -- 5. Item
            { dotProduction = DotProduction (augNT "T") [augT' "a"] [],
              suffix = [augT "a"],
              prefix = [LeftTurnstile],
              shortestPrefix = [augNT' "E'", leftTurnstile']
            },
          Item -- 6. Item
            { dotProduction = DotProduction (augNT "T") [augT' "a"] [],
              suffix = [augT "a"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item -- 5. Item
            { dotProduction = DotProduction (augNT "T") [augT' "[", augNT' "E", augT' "]"] [],
              suffix = [augT "]"],
              prefix = [LeftTurnstile],
              shortestPrefix = [augNT' "E'", leftTurnstile']
            },
          Item -- 6. Item
            { dotProduction = DotProduction (augNT "T") [augT' "[", augNT' "E", augT' "]"] [],
              suffix = [augT "]"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            }
        ],
      Set.fromList -- = ⊢
        [ Item -- 1. Item
            { dotProduction = DotProduction Start [] [rightTurnstile', augNT' "E", leftTurnstile'],
              suffix = [],
              prefix = [RightTurnstile],
              shortestPrefix = [rightTurnstile']
            }
        ],
      Set.fromList -- = T
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "E") [] [augNT' "T", augNT' "E'"],
              suffix = [RightTurnstile],
              prefix = [augT "a"],
              shortestPrefix = [augNT' "T"]
            },
          Item
            { dotProduction = DotProduction (augNT "E") [] [augNT' "T", augNT' "E'"],
              suffix = [augT "["],
              prefix = [augT "a"],
              shortestPrefix = [augNT' "T"]
            },
          Item
            { dotProduction = DotProduction (augNT "E") [] [augNT' "T", augNT' "E'"],
              suffix = [RightTurnstile],
              prefix = [augT "["],
              shortestPrefix = [augNT' "T"]
            },
          Item
            { dotProduction = DotProduction (augNT "E") [] [augNT' "T", augNT' "E'"],
              suffix = [augT "["],
              prefix = [augT "["],
              shortestPrefix = [augNT' "T"]
            },
          Item -- 2. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+"] [augNT' "T", augNT' "E'"],
              suffix = [augT "+"],
              prefix = [augT "a"],
              shortestPrefix = [augNT' "T"]
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+"] [augNT' "T", augNT' "E'"],
              suffix = [augT "+"],
              prefix = [augT "["],
              shortestPrefix = [augNT' "T"]
            }
        ],
      Set.fromList -- = a
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "T") [] [augT' "a"],
              suffix = [RightTurnstile],
              prefix = [augT "a"],
              shortestPrefix = [augT' "a"]
            },
          Item
            { dotProduction = DotProduction (augNT "T") [] [augT' "a"],
              suffix = [augT "+"],
              prefix = [augT "a"],
              shortestPrefix = [augT' "a"]
            },
          Item
            { dotProduction = DotProduction (augNT "T") [] [augT' "a"],
              suffix = [augT "["],
              prefix = [augT "a"],
              shortestPrefix = [augT' "a"]
            }
        ],
      Set.fromList -- = ]
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "T") [augT' "[", augNT' "E"] [augT' "]"],
              suffix = [augT "a"],
              prefix = [augT "]"],
              shortestPrefix = [augT' "]"]
            },
          Item
            { dotProduction = DotProduction (augNT "T") [augT' "[", augNT' "E"] [augT' "]"],
              suffix = [augT "]"],
              prefix = [augT "]"],
              shortestPrefix = [augT' "]"]
            },
          Item -- 2. Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T", augNT' "E'"] [],
              suffix = [augT "a"],
              prefix = [augT "]"],
              shortestPrefix = [augT' "]"]
            },
          Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T", augNT' "E'"] [],
              suffix = [augT "]"],
              prefix = [augT "]"],
              shortestPrefix = [augT' "]"]
            },
          Item -- 3. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T", augNT' "E'"] [],
              suffix = [augT "a"],
              prefix = [augT "]"],
              shortestPrefix = [augT' "]"]
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T", augNT' "E'"] [],
              suffix = [augT "]"],
              prefix = [augT "]"],
              shortestPrefix = [augT' "]"]
            },
          Item -- 4. Item
            { dotProduction = DotProduction (augNT "E'") [] [],
              suffix = [augT "a"],
              prefix = [augT "]"],
              shortestPrefix = [augT' "]"]
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [] [],
              suffix = [augT "]"],
              prefix = [augT "]"],
              shortestPrefix = [augT' "]"]
            }
        ],
      Set.fromList -- = +
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "E'") [] [augT' "+", augNT' "T", augNT' "E'"],
              suffix = [augT "a"],
              prefix = [augT "+"],
              shortestPrefix = [augT' "+"]
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [] [augT' "+", augNT' "T", augNT' "E'"],
              suffix = [augT "]"],
              prefix = [augT "+"],
              shortestPrefix = [augT' "+"]
            }
        ],
      Set.fromList -- = E2
        [ Item -- 1. Item (I believe the E -> [.E] from the paper is an error so I use T -> [.E].)
            { dotProduction = DotProduction (augNT "T") [augT' "["] [augNT' "E", augT' "]"],
              suffix = [augT "["],
              prefix = [augT "a"],
              shortestPrefix = [augNT' "E"]
            },
          Item
            { dotProduction = DotProduction (augNT "T") [augT' "["] [augNT' "E", augT' "]"],
              suffix = [augT "["],
              prefix = [augT "["],
              shortestPrefix = [augNT' "E"]
            }
        ],
      Set.fromList -- = E'2
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = [augT "a"],
              prefix = [augT "]"],
              shortestPrefix = [augNT' "E'", augT' "]"]
            },
          Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = [augT "]"],
              prefix = [augT "]"],
              shortestPrefix = [augNT' "E'", augT' "]"]
            },
          Item -- 2. Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = [augT "a"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item
            { dotProduction = DotProduction (augNT "E") [augNT' "T"] [augNT' "E'"],
              suffix = [augT "]"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item -- 3. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = [augT "a"],
              prefix = [augT "]"],
              shortestPrefix = [augNT' "E'", augT' "]"]
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = [augT "]"],
              prefix = [augT "]"],
              shortestPrefix = [augNT' "E'", augT' "]"]
            },
          Item -- 4. Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = [augT "a"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item
            { dotProduction = DotProduction (augNT "E'") [augT' "+", augNT' "T"] [augNT' "E'"],
              suffix = [augT "]"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item -- 5. Item
            { dotProduction = DotProduction (augNT "T") [augT' "a"] [],
              suffix = [augT "a"],
              prefix = [augT "]"],
              shortestPrefix = [augNT' "E'", augT' "]"]
            },
          Item -- 6. Item
            { dotProduction = DotProduction (augNT "T") [augT' "a"] [],
              suffix = [augT "a"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            },
          Item -- 7. Item
            { dotProduction = DotProduction (augNT "T") [augT' "[", augNT' "E", augT' "]"] [],
              suffix = [augT "]"],
              prefix = [augT "]"],
              shortestPrefix = [augNT' "E'", augT' "]"]
            },
          Item -- 8. Item
            { dotProduction = DotProduction (augNT "T") [augT' "[", augNT' "E", augT' "]"] [],
              suffix = [augT "]"],
              prefix = [augT "+"],
              shortestPrefix = [augNT' "E'"]
            }
        ],
      Set.fromList -- = [
        [ Item -- 1. Item
            { dotProduction = DotProduction (augNT "T") [] [augT' "[", augNT' "E", augT' "]"],
              suffix = [RightTurnstile],
              prefix = [augT "["],
              shortestPrefix = [augT' "["]
            },
          Item
            { dotProduction = DotProduction (augNT "T") [] [augT' "[", augNT' "E", augT' "]"],
              suffix = [augT "+"],
              prefix = [augT "["],
              shortestPrefix = [augT' "["]
            },
          Item
            { dotProduction = DotProduction (augNT "T") [] [augT' "[", augNT' "E", augT' "]"],
              suffix = [augT "["],
              prefix = [augT "["],
              shortestPrefix = [augT' "["]
            }
        ]
    ]

augmentGrammarTestCase = TestCase $ assertEqual "Augment grammar test" expected result
  where
    sortGrammar (Grammar s t nt ps) = Grammar s (List.sort t) (List.sort nt) (List.sort ps)
    augmented_grammar = augmentGrammar grammar
    expected = sortGrammar augmentedGrammar
    result = sortGrammar augmented_grammar

collectionTestCase = TestCase $ assertEqual "LLP collection test" expected result
  where
    expected = Set.empty
    computed_collection = llpCollection 1 1 grammar
    result = Set.difference computed_collection collection

pslsTestCase = TestCase $ assertEqual "PSLS table test" expected result
  where
    expected = pslsTable
    collection' = llpCollection 1 1 grammar
    result = psls collection'

tests =
  TestLabel "LLP(q, k) tests" $
    TestList
      [ augmentGrammarTestCase,
        pslsTestCase,
        collectionTestCase
      ]