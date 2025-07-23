module Main where

import CFG qualified
import Control.Monad
import LL qualified
import LLP qualified
import RegularExpression qualified
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperties)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ LL.tests,
        LLP.tests,
        RegularExpression.tests,
        CFG.properties
      ]
