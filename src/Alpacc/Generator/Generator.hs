module Alpacc.Generator.Generator
  ( Generator (..),
  )
where

import Alpacc.CFG
import Data.Text (Text)

data Generator = Generator
  { lexerParserGenerator :: Int -> Int -> CFG -> Either Text Text,
    parserGenerator :: Int -> Int -> CFG -> Either Text Text,
    lexerGenerator :: CFG -> Either Text Text
  }
