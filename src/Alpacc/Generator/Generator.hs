module Alpacc.Generator.Generator
  ( Generator (..),
  )
where

import Alpacc.CFG

data Generator = Generator
  { lexerParserGenerator :: Int -> Int -> CFG -> Either String String,
    parserGenerator :: Int -> Int -> CFG -> Either String String,
    lexerGenerator :: CFG -> Either String String
  }
