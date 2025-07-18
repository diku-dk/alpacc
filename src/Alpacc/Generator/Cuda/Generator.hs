module Alpacc.Generator.Cuda.Generator
  ( generator,
  )
where

import Alpacc.Generator.Analyzer
import Alpacc.Generator.Cuda.Lexer qualified as Lexer
import Alpacc.Generator.Cuda.Parser qualified as Parser
import Data.FileEmbed
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text

common :: Text
common = $(embedStringFile "cuda/common.cu")

auxiliary :: Analyzer [Text] -> Text
auxiliary analyzer =
  case analyzerKind analyzer of
    Lex lexer ->
      Text.unlines
        [ Text.unlines (("// " <>) <$> meta analyzer),
          common,
          Lexer.generateLexer terminal_type lexer,
          Text.pack
            [i|
int main(int32_t argc, char *argv[]) {
  return lexer_stream<WriteAscii>(WriteAscii());
}|]
        ]
    Parse parser ->
      Text.unlines
        [ Text.unlines (("// " <>) <$> meta analyzer),
          common,
          Parser.generateParser terminal_type parser,
          Text.pack
            [i|
int main(int32_t argc, char *argv[]) {
  return lexer_stream<WriteAscii>(WriteAscii());
}|]
        ]
    Both lexer parser ->
      Text.unlines
        [ Text.unlines (("// " <>) <$> meta analyzer),
          common,
          Lexer.generateLexer terminal_type lexer,
          Parser.generateParser terminal_type parser,
          Text.pack
            [i|
int main(int32_t argc, char *argv[]) {
  return lexer_stream<WriteAscii>(WriteAscii());
}|]
        ]
  where
    terminal_type = terminalType analyzer

generator :: Generator [Text]
generator =
  Generator
    { generate = auxiliary
    }
