module Alpacc.Generator.Cuda.Generator
  ( generator,
  )
where

import Alpacc.Generator.Generator

generator :: Generator
generator =
  Generator
    { lexerParserGenerator = error "Not implemented",
      lexerGenerator = error "Not implemented",
      parserGenerator = error "Not implemented"
    }
