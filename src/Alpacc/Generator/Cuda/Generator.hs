module Alpacc.Generator.Cuda.Generator
  ( generator,
  )
where

import Alpacc.CFG
import Alpacc.Generator.Cuda.Lexer qualified as Lexer
import Alpacc.Generator.Generator
import Alpacc.Grammar
import Alpacc.Types
import Data.Either.Extra
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)

generateLexer :: CFG -> Either String String
generateLexer cfg = do
  t_rules <- everyTRule cfg
  let terminal_index_map = toTerminalIndexMap (ruleT <$> t_rules)
  terminal_type :: IInt <-
    maybeToEither "Error: Too many terminals." $
      toIntType $
        fromIntegral $
          Map.size terminal_index_map
  lexer <- cfgToDFALexer cfg
  lexer_str <- Lexer.generateLexer lexer terminal_index_map terminal_type
  return $
    unlines
      [ lexer_str
      ]

generator :: Generator
generator =
  Generator
    { lexerParserGenerator = error "Not implemented",
      lexerGenerator = generateLexer,
      parserGenerator = error "Not implemented"
    }
