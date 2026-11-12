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
futharkTest = $(embedStringFile "backends/futhark/test.fut")

idxModule :: Bool -> Text
idxModule index32 = "module idx = " <> if index32 then "i32" else "i64"

lenModule :: Bool -> Maybe UInt -> Text
lenModule index32 mLenType = "module len = " <> case mLenType of
  Nothing  -> if index32 then "u32" else "u64"
  Just U8  -> "u8"
  Just U16 -> "u16"
  Just U32 -> "u32"
  Just U64 -> "u64"

bothFunction :: UInt -> UInt -> Text
bothFunction terminal_type production_type =
  Text.strip $
    Text.pack
      [i|
entry parse s =
  let tokens' = lexer.lex_int s
  let tokens =
    match tokens'
    case #some t -> t
    case #none -> []
  let res = parser.parse (map (.0) tokens)
  let (tree, token_parents) =
    match res
    case #some r -> r
    case #none -> ([], map (const (idx.i64 0)) tokens)
  let result : opt ( [](parser.terminal_int, (idx.t, len.t))
                   , [](parser.production, idx.t)
                   , []idx.t
                   ) =
    if opt.is_some tokens' && opt.is_some res
    then #some (tokens, tree, token_parents)
    else #none
  in result

entry parse_int s =
  let tokens' = lexer.lex_int s
  let tokens =
    match tokens'
    case #some t -> t
    case #none -> []
  let res = parser.parse_int (map (.0) tokens)
  let (tree, token_parents) =
    match res
    case #some r -> r
    case #none -> ([], map (const (idx.i64 0)) tokens)
  let result : opt ( [](parser.terminal_int, (idx.t, len.t))
                   , [](parser.production_int, idx.t)
                   , []idx.t
                   ) =
    if opt.is_some tokens' && opt.is_some res
    then #some (tokens, tree, token_parents)
    else #none
  in result

module tester = lexer_parser_test {
  type terminal_int = parser.terminal_int
  type production_int = parser.production_int
  def parse_int = parse_int
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
  match lexer.lex s
  case #some r -> let (tokens, spans) = unzip r
                  let (starts, lengths) = unzip spans
                  in (tokens, starts, lengths)
  case #none -> ([], [], [])

entry lex_int s =
  match lexer.lex_int s
  case #some r -> let (tokens, spans) = unzip r
                  let (starts, lengths) = unzip spans
                  in (tokens, starts, lengths)
  case #none -> ([], [], [])


entry test [n] (s: [n]u8) : []u8 = tester.test s
|]

parserFunction :: UInt -> UInt -> Text
parserFunction terminal_type production_type =
  Text.strip $
    Text.pack
      [i|
module tester = parser_test parser #{futharkify terminal_type} #{futharkify production_type}

entry parse = parser.parse

entry parse_int = parser.parse_int

entry test [n] (s: [n]u8) : []u8 = tester.test s
|]

terminalNameType :: [Text] -> Text
terminalNameType names =
  "type terminal = " <> Text.intercalate " | " names

numberOfterminals :: [Text] -> Text
numberOfterminals names =
  "def number_of_terminals : i64 = " <> futharkify (length names)

terminalIntToName :: [Text] -> Text
terminalIntToName names =
  "def terminal_int_to_name : [number_of_terminals]terminal = "
    <> futharkify (map RawString names)
    <> " :> [number_of_terminals]terminal"

terminalDefinitions :: [Text] -> Text
terminalDefinitions names =
  Text.unlines
    [ terminalNameType names,
      numberOfterminals names,
      terminalIntToName names
    ]

auxiliary :: Bool -> Maybe UInt -> Analyzer [Text] -> Text
auxiliary index32 mLenType analyzer =
  case analyzerKind analyzer of
    Lex lexer ->
      Text.unlines
        [ Text.unlines (("-- " <>) <$> meta analyzer),
          idxModule index32,
          lenModule index32 mLenType,
          terminalDefinitions terminal_names,
          Lexer.generateLexer terminal_type lexer,
          futharkTest,
          lexerFunction terminal_type
        ]
    Parse parser ->
      Text.unlines
        [ Text.unlines (("-- " <>) <$> meta analyzer),
          idxModule index32,
          lenModule index32 mLenType,
          terminalDefinitions terminal_names,
          Parser.generateParser terminal_type parser,
          futharkTest,
          parserFunction terminal_type (productionType parser)
        ]
    Both lexer parser ->
      Text.unlines
        [ Text.unlines (("-- " <>) <$> meta analyzer),
          idxModule index32,
          lenModule index32 mLenType,
          terminalDefinitions terminal_names,
          Lexer.generateLexer terminal_type lexer,
          Parser.generateParser terminal_type parser,
          futharkTest,
          bothFunction terminal_type (productionType parser)
        ]
  where
    terminal_type = terminalType analyzer
    terminal_names = ("#" <>) <$> terminalToName analyzer

generator :: Bool -> Maybe UInt -> Generator [Text]
generator index32 mLenType =
  Generator
    { generate = auxiliary index32 mLenType
    }
