module Alpacc.Generator.Generator
  ( Analyzer (..),
    Lexer (..),
    Parser (..),
    AnalyzerKind (..),
    CodeGenerator (..),
    mkLexer,
    mkParser,
    mkLexerParser,
  )
where

import Alpacc.CFG
import Alpacc.Generator.Util
import Alpacc.Grammar
import Alpacc.HashTable
import Alpacc.LLP (Bracket)
import Alpacc.Lexer.DFAParallelLexer
import Alpacc.Lexer.Encode
import Alpacc.Lexer.ParallelLexing
import Data.Either.Extra
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Word

data CodeGenerator
  = CodeGenerator
  { generate :: Analyzer -> Either Text Text
  }

data Lexer
  = Lexer
  { stateType :: UInt,
    lexer :: IntParallelLexer Word8 Integer,
    ignoreToken :: Maybe Integer,
    deadToken :: Integer,
    transitionToState :: [Integer]
  }
  deriving (Show)

transitionToStateArray :: IntParallelLexer Word8 Integer -> Either Text [Integer]
transitionToStateArray parallel_lexer =
  maybeToEither "test" $
    mapM
      (`Map.lookup` to_endo)
      [0 .. 255]
  where
    to_endo = endomorphisms $ parLexer parallel_lexer

data Parser
  = Parser
  { startTerminal :: Integer,
    endTerminal :: Integer,
    lookback :: Int,
    lookahead :: Int,
    bracketType :: UInt,
    productionType :: UInt,
    llpTable :: HashTableMem Integer (Bracket Integer) Int,
    arities :: [Integer],
    numberOfProductions :: Int,
    numberOfTerminals :: Int
  }
  deriving (Show)

data AnalyzerKind
  = Parse Parser
  | Lex Lexer
  | Both Lexer Parser
  deriving (Show)

data Analyzer
  = Analyzer
  { terminalType :: UInt,
    generatorKind :: AnalyzerKind
  }
  deriving (Show)

mkLexer :: CFG -> Either Text Analyzer
mkLexer cfg = do
  t_rules <- everyTRule cfg
  let terminal_map = toTerminalIndexMap ((Just . ruleT <$> t_rules) ++ [Nothing])
      dead_token = terminal_map Map.! Nothing
  terminal_type <- findTerminalIntType terminal_map
  lexer <- cfgToDFALexer cfg
  parallel_lexer <- intDfaParallelLexer terminal_map lexer
  state_type <- extEndoType $ parLexer parallel_lexer
  transition_to_state <- transitionToStateArray parallel_lexer
  pure $
    Analyzer
      { generatorKind =
          Lex $
            Lexer
              { stateType = state_type,
                lexer = parallel_lexer,
                deadToken = dead_token,
                transitionToState = transition_to_state,
                ignoreToken = Map.lookup (Just (T "ignore")) terminal_map
              },
        terminalType = terminal_type
      }

mkArities :: Grammar nt t -> [Integer]
mkArities = fmap arity . productions
  where
    arity = sum . fmap isNt . symbols
    isNt (Nonterminal _) = 1 :: Integer
    isNt _ = 0

mkParser :: Int -> Int -> CFG -> Either Text Analyzer
mkParser q k cfg = do
  grammar <- augmentGrammar . extendByTerminals <$> cfgToGrammar cfg
  let symbol_index_map = toSymbolIndexMap (terminals grammar) (nonterminals grammar)
      terminal_map = toTerminalIndexMap $ terminals grammar
  terminal_type <- findTerminalIntType terminal_map
  (start_terminal, end_terminal) <- startEndIndex symbol_index_map
  bracket_type <- findBracketIntType symbol_index_map
  production_type <- findProductionIntType grammar
  hash_table <- llpHashTable q k U64 terminal_type grammar symbol_index_map
  pure $
    Analyzer
      { generatorKind =
          Parse $
            Parser
              { startTerminal = start_terminal,
                endTerminal = end_terminal,
                bracketType = bracket_type,
                productionType = production_type,
                llpTable = hash_table,
                arities = mkArities grammar
              },
        terminalType = terminal_type
      }

addDeadToken :: Grammar nt t -> Grammar nt (Maybe t)
addDeadToken g = g' {terminals = terminals g' ++ [Nothing]}
  where
    g' = Just <$> g

mkLexerParser :: Int -> Int -> CFG -> Either Text Analyzer
mkLexerParser q k cfg = do
  grammar <-
    augmentGrammar
      . extendByTerminals
      . addDeadToken
      <$> cfgToGrammar cfg
  let symbol_index_map = toSymbolIndexMap (terminals grammar) (nonterminals grammar)
      terminal_map = toTerminalIndexMap (terminals grammar)
      dead_token = terminal_map Map.! (AugmentedTerminal Nothing)
  terminal_type <- findAugmentedTerminalIntType grammar
  (start_terminal, end_terminal) <- startEndIndex symbol_index_map
  bracket_type <- findBracketIntType symbol_index_map
  production_type <- findProductionIntType grammar
  hash_table <- llpHashTable q k U64 terminal_type grammar symbol_index_map
  lexer <- cfgToDFALexer cfg
  parallel_lexer <- intDfaParallelLexer terminal_map lexer
  state_type <- extEndoType $ parLexer parallel_lexer
  transition_to_state <- transitionToStateArray parallel_lexer
  pure $
    Analyzer
      { generatorKind =
          Both
            ( Lexer
                { stateType = state_type,
                  lexer = parallel_lexer,
                  deadToken = dead_token,
                  transitionToState = transition_to_state,
                  ignoreToken = Map.lookup (AugmentedTerminal (Just (T "ignore"))) terminal_map
                }
            )
            ( Parser
                { startTerminal = start_terminal,
                  endTerminal = end_terminal,
                  bracketType = bracket_type,
                  productionType = production_type,
                  llpTable = hash_table,
                  arities = mkArities grammar
                }
            ),
        terminalType = terminal_type
      }
