module Alpacc.Generator.Futhark.Generator
  ( generator,
  )
where

import Alpacc.Generator.Analyzer
import Alpacc.Generator.Futhark.Lexer qualified as Lexer
import Alpacc.Generator.Futhark.Parser qualified as Parser
import Data.FileEmbed
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text

futharkTest :: Text
futharkTest = $(embedStringFile "futhark/test.fut")

parentVectorTest :: Text
parentVectorTest =
  Text.strip $
    Text.pack
      [i|
-- ==
-- entry: test_previous_equal_or_smaller
-- compiled random input { [100]i32 }
-- output { true }
entry test_previous_equal_or_smaller [n] (arr: [n]i32): bool =
  parser.test_previous_equal_or_smaller arr
|]

bothFunction :: Text
bothFunction =
  Text.strip $
    Text.pack
      [i|
entry parse s =
  match lexer.lex 16777216 s
  case #some r -> parser.parse r
  case #none -> []

entry productions s =
  match lexer.lex 16777216 s
  case #some r -> map (.0) r |> parser.productions 
  case #none -> []

entry pre_productions s =
  match lexer.lex 16777216 s
  case #some r -> map (.0) r |> parser.pre_productions 
  case #none -> []
|]
      <> parentVectorTest

lexerFunction :: Text
lexerFunction =
  Text.strip $
    Text.pack
      [i|
module tester = lexer_test lexer u8

entry lex s =
  match lexer.lex 16777216 s
  case #some r -> let (tokens, spans) = unzip r
                  let (starts, ends) = unzip spans
                  in (tokens, starts, ends)
  case #none -> ([], [], [])

entry test [n] (s: [n]u8) : []u8 = lexer.test 16777216 s
|]

parserFunction :: Text
parserFunction =
  Text.strip $
    Text.pack
      [i|
module tester = lexer_test lexer u8

entry parse = parser.parse
|]
      <> parentVectorTest

auxiliary :: Analyzer [Text] -> Text
auxiliary analyzer =
  case analyzerKind analyzer of
    Lex lexer ->
      Text.unlines
        [ Text.unlines (("-- " <>) <$> meta analyzer),
          Lexer.generateLexer terminal_type lexer,
          futharkTest,
          lexerFunction
        ]
    Parse parser ->
      Text.unlines
        [ Text.unlines (("-- " <>) <$> meta analyzer),
          Parser.generateParser terminal_type parser,
          futharkTest,
          parserFunction
        ]
    Both lexer parser ->
      Text.unlines
        [ Text.unlines (("-- " <>) <$> meta analyzer),
          Lexer.generateLexer terminal_type lexer,
          Parser.generateParser terminal_type parser,
          futharkTest,
          bothFunction
        ]
  where
    terminal_type = terminalType analyzer

generator :: Generator [Text]
generator =
  Generator
    { generate = auxiliary
    }
