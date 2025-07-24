module Alpacc.Random
  ( randomLexer,
  )
where

import Alpacc.CFG
import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Data.Text (Text)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Gen,
    Property,
    elements,
    generate,
    listOf,
    oneof,
    property,
    sized,
  )

randomLexer :: IO Text
randomLexer = printDfaSpec <$> generate arbitrary
