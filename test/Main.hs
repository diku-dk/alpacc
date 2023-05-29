module Main where

import qualified LL
import qualified LLP
import Test.HUnit

tests =
  TestList
    [ LL.tests,
      LLP.tests
    ]

main :: IO ()
main = do
  runTestTTAndExit tests