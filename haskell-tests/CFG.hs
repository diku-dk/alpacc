module CFG (properties) where

import Alpacc.CFG qualified
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)

properties :: TestTree
properties = testProperties "Lexer properties" Alpacc.CFG.properties
