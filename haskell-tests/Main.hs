module Main where

import qualified LL
import qualified LLP
import qualified RegularExpression
import Test.HUnit

tests =
  TestList
    [ LL.tests,
      LLP.tests,
      RegularExpression.tests
    ]

main :: IO ()
main = do
  runTestTTAndExit tests
