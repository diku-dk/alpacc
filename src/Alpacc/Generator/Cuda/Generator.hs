module Alpacc.Generator.Cuda.Generator
  ( generator,
  )
where

import Alpacc.CFG
import Alpacc.Generator.Cuda.Lexer qualified as Lexer
import Alpacc.Generator.Cuda.Parser qualified as Parser
import Alpacc.Generator.Generator
import Alpacc.Grammar
import Alpacc.Types
import Data.Either.Extra
import Data.FileEmbed
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text

common :: Text
common = $(embedStringFile "cuda/common.cu")

generate :: Int -> Int -> CFG -> Either Text Text
generate q k cfg = do
  grammar <- extendByTerminals <$> cfgToGrammar cfg
  let terminal_index_map = toTerminalIndexMap $ terminals grammar
  let ts = terminals grammar
  let nts = nonterminals grammar
  let symbol_index_map = toSymbolIndexMap ts nts
  (parser, terminal_type) <- Parser.generateParser q k grammar symbol_index_map
  lexer <- cfgToDFALexer cfg
  lexer_str <- Lexer.generateLexer lexer terminal_index_map terminal_type
  pure $
    Text.unlines
      [ common,
        lexer_str,
        parser,
        Text.strip $
          Text.pack
            [i|
int main(int32_t argc, char *argv[]) {
  return lexer_stream<WriteAscii>(WriteAscii());
}|]
      ]

generateLexer :: CFG -> Either Text Text
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
  pure $
    Text.unlines
      [ common,
        lexer_str,
        Text.strip $
          Text.pack
            [i|
int main(int32_t argc, char *argv[]) {
  return lexer_stream<WriteAscii>(WriteAscii());
}|]
      ]

generator :: Generator
generator =
  Generator
    { lexerParserGenerator = generate,
      lexerGenerator = generateLexer,
      parserGenerator = error "Not implemented"
    }
