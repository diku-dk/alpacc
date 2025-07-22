module CFG (properties) where

import Alpacc.CFG qualified
import Test.Tasty.QuickCheck (testProperties)

properties = testProperties "APL properties" Alpacc.CFG.properties
