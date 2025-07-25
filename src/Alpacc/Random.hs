module Alpacc.Random
  ( randomLexer,
  )
where

import Alpacc.CFG
import Data.Text (Text)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    generate,
  )

randomLexer :: IO Text
randomLexer = printDfaSpec <$> generate arbitrary
