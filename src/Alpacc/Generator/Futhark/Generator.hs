module Alpacc.Generator.Futhark.Generator
  ( generator,
  )
where

import Alpacc.CFG
import Alpacc.Generator.Futhark.Futharkify
import Alpacc.Generator.Futhark.Lexer qualified as Lexer
import Alpacc.Generator.Futhark.Parser qualified as Parser
import Alpacc.Generator.Generator
import Alpacc.Grammar
import Alpacc.Types
import Data.Either.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Interpolate (i)

parentVectorTest :: String
parentVectorTest =
  [i|
-- ==
-- entry: test_previous_equal_or_smaller
-- compiled random input { [100]i32 }
-- output { true }
entry test_previous_equal_or_smaller [n] (arr: [n]i32): bool =
  parser.test_previous_equal_or_smaller arr
|]

bothFunction :: String
bothFunction =
  [i|
entry parse s =
  match lexer.lex_chunked 16777216 s
  case #some r -> parser.parse r
  case #none -> []

entry productions s =
  match lexer.lex_chunked 16777216 s
  case #some r -> map (.0) r |> parser.productions 
  case #none -> []

entry pre_productions s =
  match lexer.lex_chunked 16777216 s
  case #some r -> map (.0) r |> parser.pre_productions 
  case #none -> []
|]
    ++ parentVectorTest

lexerFunction :: IInt -> String
lexerFunction t =
  [i|
entry lex s =
  match lexer.lex_chunked 16777216 s
  case #some r ->
    map (\\(a, (b, c)) -> [i32.#{futharkify t} a, b, c]) r
  case #none -> []
|]

parserFunction :: String
parserFunction =
  [i|
entry parse = parser.parse
|]
    ++ parentVectorTest

toTerminalIndexMap :: (Ord t) => [t] -> Map t Int
toTerminalIndexMap = Map.fromList . flip zip [0 ..]

toSymbolIndexMap ::
  (Ord t, Ord nt) =>
  [t] ->
  [nt] ->
  Map (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)) Int
toSymbolIndexMap ts nts = Map.union aug_terminal_map nts_map
  where
    terminal_map = Map.mapKeys AugmentedTerminal $ toTerminalIndexMap ts
    max_index = maximum terminal_map
    new_terminals =
      Map.union terminal_map $
        Map.fromList
          [ (LeftTurnstile, max_index + 1),
            (RightTurnstile, max_index + 2)
          ]
    aug_terminal_map = Map.mapKeys Terminal new_terminals
    nts' = (++ [Nonterminal Start]) $ Nonterminal . AugmentedNonterminal <$> nts
    nts_map = Map.fromList $ zip nts' [max_index + 3 ..]

generate :: Int -> Int -> CFG -> Either String String
generate q k cfg = do
  grammar <- extendByTerminals <$> cfgToGrammar cfg
  let terminal_index_map = toTerminalIndexMap $ terminals grammar
  let ts = terminals grammar
  let nts = nonterminals grammar
  let symbol_index_map = toSymbolIndexMap ts nts
  (parser, terminal_type) <- Parser.generateParser q k grammar symbol_index_map
  lexer <- cfgToDFALexer cfg
  lexer_str <- Lexer.generateLexer lexer terminal_index_map terminal_type
  return $
    unlines
      [ parser,
        lexer_str,
        bothFunction
      ]

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
      [ lexer_str,
        lexerFunction terminal_type
      ]

generateParser :: Int -> Int -> CFG -> Either String String
generateParser q k cfg = do
  grammar <- extendByTerminals <$> cfgToGrammar cfg
  let symbol_index_map = toSymbolIndexMap (terminals grammar) (nonterminals grammar)
  (parser, _) <- Parser.generateParser q k grammar symbol_index_map
  return $
    unlines
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
