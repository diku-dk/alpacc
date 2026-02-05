module Alpacc.Generator.Analyzer
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
import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Alpacc.Lexer.DFAParallelLexer
import Alpacc.Lexer.Encode
import Alpacc.Lexer.ParallelLexing
import Alpacc.Types
import Data.Either.Extra
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text hiding (Text)
import Data.Word

data Generator a
  = Generator
  { generate :: Analyzer a -> Text
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
    llpTable :: LLPTable,
    arities :: [Integer],
    productionToTerminal :: [Maybe Integer],
    numberOfProductions :: Int,
    productionToName :: [Text]
  }
  deriving (Show)

data AnalyzerKind
  = Parse Parser
  | Lex Lexer
  | Both Lexer Parser
  deriving (Show)

data Analyzer a
  = Analyzer
  { terminalType :: UInt,
    analyzerKind :: AnalyzerKind,
    meta :: a
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

mkLexer :: CFG -> Either Text (Analyzer [Text])
mkLexer cfg = do
  spec <- cfgToDFALexerSpec cfg
  let ignore = T "ignore"
      encoder = encodeTerminals ignore $ parsingTerminals $ dfaTerminals spec
      dfa = lexerDFA (0 :: Integer) $ dfaCharToWord8 spec
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
        terminalType = terminal_type,
        meta = printTerminals ignore encoder
      }

mkArities :: ParsingGrammar nt t -> [Integer]
mkArities = fmap arity . productions . getGrammar
  where
    arity = sum . fmap isNt . symbols
    isNt (Nonterminal _) = 1 :: Integer
    isNt _ = 0

nameProduction :: Int -> AugmentedNonterminal (Symbol NT T) -> Text
nameProduction int Start = "start_" <> Text.pack (show int)
nameProduction int (AugmentedNonterminal (Nonterminal (NT nt))) =
  nt <> "_" <> Text.pack (show int)
nameProduction int (AugmentedNonterminal (Terminal (T t))) =
  t <> "_" <> Text.pack (show int)
nameProduction int (AugmentedNonterminal (Terminal (TLit _))) =
  "literal_" <> Text.pack (show int)

mkParser :: Int -> Int -> CFG -> Either Text (Analyzer [Text])
mkParser q k cfg = do
  grammar <- cfgToGrammar cfg
  let ignore = T "ignore"
      s_encoder = encodeSymbols ignore grammar
      t_encoder = fromSymbolToTerminalEncoder s_encoder
      production_to_terminal = mkProductionToTerminal t_encoder grammar
      start_terminal = symbolStartTerminal s_encoder
      end_terminal = symbolEndTerminal s_encoder
      empty_terminal = symbolDead s_encoder
      production_to_names = productionNames nameProduction grammar
  terminal_type <- symbolTerminalIntType s_encoder
  bracket_type <- bracketIntType s_encoder
  production_type <- productionIntType grammar
  hash_table <- llpHashTable q k empty_terminal grammar s_encoder
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
                numberOfProductions = length $ productions $ getGrammar grammar,
                productionToName = production_to_names
              },
        terminalType = terminal_type,
        meta =
          printTerminals ignore t_encoder
            <> [""]
            <> printProductions production_to_names grammar
      }

mkLexerParser :: Int -> Int -> CFG -> Either Text (Analyzer [Text])
mkLexerParser q k cfg = do
  grammar <- cfgToGrammar cfg
  spec <- cfgToDFALexerSpec cfg
  let ignore = T "ignore"
      s_encoder = encodeSymbols ignore grammar
      t_encoder = fromSymbolToTerminalEncoder s_encoder
      production_to_terminal = mkProductionToTerminal t_encoder grammar
      start_terminal = symbolStartTerminal s_encoder
      end_terminal = symbolEndTerminal s_encoder
      empty_terminal = symbolDead s_encoder
      dead_token = empty_terminal
      dfa = lexerDFA (0 :: Integer) $ dfaCharToWord8 spec
      production_to_names = productionNames nameProduction grammar
  terminal_type <- symbolTerminalIntType s_encoder
  bracket_type <- bracketIntType s_encoder
  production_type <- productionIntType grammar
  hash_table <- llpHashTable q k empty_terminal grammar s_encoder
  parallel_lexer <- intDfaParallelLexer t_encoder dfa
  state_type <- stateIntType (parLexer parallel_lexer) t_encoder
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
                  ignoreToken = terminalLookup ignore t_encoder
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
                  numberOfProductions = length $ productions $ getGrammar grammar,
                  productionToTerminal = production_to_terminal,
                  llpTable = hash_table,
                  arities = mkArities grammar,
                  productionToName = production_to_names
                }
            ),
        terminalType = terminal_type,
        meta =
          printTerminals ignore t_encoder
            <> [""]
            <> printProductions production_to_names grammar
      }
