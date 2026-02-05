module Alpacc.Generator.Futhark.Generator
  ( generator,
  )
where

import Alpacc.Generator.Analyzer
import Alpacc.Generator.Futhark.Futharkify
import Alpacc.Generator.Futhark.Lexer qualified as Lexer
import Alpacc.Generator.Futhark.Parser qualified as Parser
import Alpacc.Types
import Data.FileEmbed
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text

futharkTest :: Text
futharkTest = $(embedStringFile "futhark/test.fut")

bothFunction :: UInt -> UInt -> Text
bothFunction terminal_type production_type =
  Text.strip $
    Text.pack
      [i|
entry parse s =
  let tokens' = lexer.lex 16777216 s
  let tokens =
    match tokens'
    case #some t -> t
    case #none -> []
  let cst = parser.parse tokens
  in if is_some tokens'
     then cst
     else #none

entry parse_int s =
  let tokens' = lexer.lex 16777216 s
  let tokens =
    match tokens'
    case #some t -> t
    case #none -> []
  let cst = parser.parse_int tokens
  in if is_some tokens'
     then cst
     else #none

module tester = lexer_parser_test {
  type terminal = parser.terminal
  type production = parser.production_int
  type node 't 'p = parser.node t p
  def parse = parse_int
} #{futharkify terminal_type} #{futharkify production_type}

entry test [n] (s: [n]u8) : []u8 = tester.test s
|]

lexerFunction :: UInt -> Text
lexerFunction terminal_type =
  Text.strip $
    Text.pack
      [i|
module tester = lexer_test lexer #{futharkify terminal_type}

entry lex s =
  match lexer.lex 16777216 s
  case #some r -> let (tokens, spans) = unzip r
                  let (starts, ends) = unzip spans
                  in (tokens, starts, ends)
  case #none -> ([], [], [])

entry test [n] (s: [n]u8) : []u8 = tester.test 16777216 s
|]

parserFunction :: UInt -> UInt -> Text
parserFunction terminal_type production_type =
  Text.strip $
    Text.pack
      [i|
module tester = parser_test parser #{futharkify terminal_type} #{futharkify production_type}

entry parse = parser.parse

entry test [n] (s: [n]u8) : []u8 = tester.test s
|]

auxiliary :: Analyzer [Text] -> Text
auxiliary analyzer =
  case analyzerKind analyzer of
    Lex lexer ->
      Text.unlines
        [ Text.unlines (("-- " <>) <$> meta analyzer),
          Lexer.generateLexer terminal_type lexer,
          futharkTest,
          lexerFunction terminal_type
        ]
    Parse parser ->
      Text.unlines
        [ Text.unlines (("-- " <>) <$> meta analyzer),
          Parser.generateParser terminal_type parser,
          futharkTest,
          parserFunction terminal_type (productionType parser)
        ]
    Both lexer parser ->
      Text.unlines
        [ Text.unlines (("-- " <>) <$> meta analyzer),
          Lexer.generateLexer terminal_type lexer,
          Parser.generateParser terminal_type parser,
          futharkTest,
          bothFunction terminal_type (productionType parser)
        ]
  where
    terminal_type = terminalType analyzer

generator :: Generator [Text]
generator =
  Generator
    { generate = auxiliary
    }
