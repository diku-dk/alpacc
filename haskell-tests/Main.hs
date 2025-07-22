module Main where

import CFG qualified
import Control.Monad
import LL qualified
import LLP qualified
import RegularExpression qualified
import Test.HUnit
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperties)

tests =
  TestList
    [ LL.tests,
      LLP.tests,
      RegularExpression.tests
    ]

properties =
  testGroup "Tasty tests" [CFG.properties]

main :: IO ()
main = do
  defaultMain CFG.properties
  void $ runTestTT tests
