module Alpacc.Generator.Futhark.Generator
  ( generator,
  )
where

import Alpacc.CFG
import Alpacc.Generator.Futhark.Futharkify
import Alpacc.Generator.Futhark.Lexer qualified as Lexer
import Alpacc.Generator.Futhark.Parser qualified as Parser
import Alpacc.Generator.Generator
import Alpacc.Generator.Util
import Alpacc.Grammar
import Alpacc.Types
import Data.Either.Extra
import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text

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
entry lex s =
  match lexer.lex 16777216 s
  case #some r -> unzip3 r
  case #none -> ([], [], [])
|]

parserFunction :: Text
parserFunction =
  Text.strip $
    Text.pack
      [i|
entry parse = parser.parse
|]
      <> parentVectorTest

generate :: Int -> Int -> CFG -> Either Text Text
generate q k cfg = do
  grammar <- extendByTerminals <$> cfgToGrammar cfg
  let terminal_index_map = toTerminalIndexMap $ terminals grammar
  let ts = terminals grammar
  let nts = nonterminals grammar
  let symbol_index_map = toSymbolIndexMap ts nts
  terminal_type <- findAugmentedTerminalIntType grammar
  parser <- Parser.generateParser q k terminal_type grammar symbol_index_map
  lexer <- cfgToDFALexer cfg
  lexer_str <- Lexer.generateLexer lexer terminal_index_map terminal_type
  pure $
    Text.unlines
      [ parser,
        lexer_str,
        bothFunction
      ]

generateLexer :: CFG -> Either Text Text
generateLexer cfg = do
  t_rules <- everyTRule cfg
  let terminal_index_map = toTerminalIndexMap (ruleT <$> t_rules)
  terminal_type <- findTerminalIntType terminal_index_map
  lexer <- cfgToDFALexer cfg
  lexer_str <- Lexer.generateLexer lexer terminal_index_map terminal_type
  pure $
    Text.unlines
      [ lexer_str,
        lexerFunction
      ]

generateParser :: Int -> Int -> CFG -> Either Text Text
generateParser q k cfg = do
  grammar <- extendByTerminals <$> cfgToGrammar cfg
  let symbol_index_map = toSymbolIndexMap (terminals grammar) (nonterminals grammar)
  terminal_type <- findAugmentedTerminalIntType grammar
  parser <- Parser.generateParser q k terminal_type grammar symbol_index_map
  pure $
    Text.unlines
      [ parser,
        parserFunction
      ]

generator :: Generator
generator =
  Generator
    { lexerParserGenerator = generate,
      lexerGenerator = generateLexer,
      parserGenerator = generateParser
    }
