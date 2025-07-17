module Alpacc.Generator.Generator
  ( Analyzer (..),
    Lexer (..),
    Parser (..),
    AnalyzerKind (..),
    Generator (..),
    mkLexer,
    mkParser,
    mkLexerParser,
  )
where

import Alpacc.CFG
import Alpacc.Encode
import Alpacc.Generator.Util
import Alpacc.Grammar
import Alpacc.HashTable
import Alpacc.LLP (Bracket)
import Alpacc.Lexer.DFA
import Alpacc.Lexer.DFAParallelLexer
import Alpacc.Lexer.Encode
import Alpacc.Lexer.ParallelLexing
import Alpacc.Types
import Data.Either.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Word

data Generator
  = Generator
  { generate :: Analyzer -> Text
  }

data Lexer
  = Lexer
  { stateType :: UInt,
    lexer :: IntParallelLexer Word8,
    ignoreToken :: Maybe Integer,
    deadToken :: Integer,
    transitionToState :: [Integer]
  }
  deriving (Show)

transitionToStateArray :: IntParallelLexer Word8 -> Either Text [Integer]
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
    emptyTerminal :: Integer,
    lookback :: Int,
    lookahead :: Int,
    bracketType :: UInt,
    productionType :: UInt,
    llpTable :: HashTableMem Integer (Bracket Integer) Int,
    arities :: [Integer],
    productionToTerminal :: [Maybe Integer],
    numberOfProductions :: Int
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
    analyzerKind :: AnalyzerKind
  }
  deriving (Show)

mkProductionToTerminal ::
  (Ord nt, Ord t) =>
  TerminalEncoder t ->
  ParsingGrammar nt t ->
  [Maybe Integer]
mkProductionToTerminal encoder grammar =
  p . nonterminal <$> productions (getGrammar grammar)
  where
    p (AugmentedNonterminal (Terminal t)) = x
      where
        x = terminalLookup t encoder
    p _ = Nothing

mkLexer :: CFG -> Either Text Analyzer
mkLexer cfg = do
  spec <- cfgToDFALexerSpec cfg
  let ignore = T "ignore"
  let encoder = encodeTerminals ignore $ parsingTerminals $ dfaTerminals spec
  let dfa = lexerDFA (0 :: Integer) spec
  terminal_type <- terminalIntType encoder
  parallel_lexer <- intDfaParallelLexer encoder dfa
  state_type <- stateIntType (parLexer parallel_lexer) encoder
  transition_to_state <- transitionToStateArray parallel_lexer
  pure $
    Analyzer
      { analyzerKind =
          Lex $
            Lexer
              { stateType = state_type,
                lexer = parallel_lexer,
                deadToken = terminalDead encoder,
                transitionToState = transition_to_state,
                ignoreToken = terminalLookup ignore encoder
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
  grammar <- cfgToGrammar cfg
  let ignore = T "ignore"
  let s_encoder = encodeSymbols ignore grammar
      t_encoder = fromSymbolToTerminalEncoder s_encoder
      production_to_terminal = mkProductionToTerminal t_encoder grammar
      start_terminal = symbolStartTerminal s_encoder
      end_terminal = symbolStartTerminal s_encoder
      empty_terminal = symbolDead s_encoder
  terminal_type <- symbolTerminalIntType s_encoder
  bracket_type <- bracketIntType s_encoder
  production_type <- productionIntType grammar
  hash_table <- llpHashTable q k I64 empty_terminal grammar symbol_index_map
  pure $
    Analyzer
      { analyzerKind =
          Parse $
            Parser
              { startTerminal = start_terminal,
                endTerminal = end_terminal,
                bracketType = bracket_type,
                lookback = q,
                lookahead = k,
                emptyTerminal = empty_terminal,
                productionType = production_type,
                productionToTerminal = production_to_terminal,
                llpTable = hash_table,
                arities = mkArities grammar,
                numberOfProductions = length $ productions grammar
              },
        terminalType = terminal_type
      }

mkLexerParser :: Int -> Int -> CFG -> Either Text Analyzer
mkLexerParser q k cfg = do
  grammar <- cfgToGrammar cfg
  let terminal_map = toTerminalIndexMap (terminals grammar)
  (dead_token, terminal_type) <- mkTerminalType terminal_map
  let symbol_index_map = toSymbolIndexMap terminal_map (nonterminals grammar)
      production_to_terminal = mkProductionToTerminal symbol_index_map $ productions grammar
      empty_terminal = dead_token
  (start_terminal, end_terminal) <- startEndIndex symbol_index_map
  bracket_type <- findBracketIntType symbol_index_map
  production_type <- findProductionIntType grammar
  hash_table <- llpHashTable q k I64 empty_terminal grammar symbol_index_map
  lexer <- cfgToDFALexerSpec cfg
  parallel_lexer <- intDfaParallelLexer (unaugmentTerminalMap terminal_map) dead_token undefined
  state_type <- extEndoType $ parLexer parallel_lexer
  transition_to_state <- transitionToStateArray parallel_lexer
  pure $
    Analyzer
      { analyzerKind =
          Both
            ( Lexer
                { stateType = state_type,
                  lexer = parallel_lexer,
                  deadToken = dead_token,
                  transitionToState = transition_to_state,
                  ignoreToken = Map.lookup (AugmentedTerminal (T "ignore")) terminal_map
                }
            )
            ( Parser
                { startTerminal = start_terminal,
                  endTerminal = end_terminal,
                  lookback = q,
                  lookahead = k,
                  emptyTerminal = empty_terminal,
                  bracketType = bracket_type,
                  productionType = production_type,
                  numberOfProductions = length $ productions grammar,
                  productionToTerminal = production_to_terminal,
                  llpTable = hash_table,
                  arities = mkArities grammar
                }
            ),
        terminalType = terminal_type
      }
