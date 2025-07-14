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
import Alpacc.Generator.Util
import Alpacc.Grammar
import Alpacc.HashTable
import Alpacc.LLP (Bracket)
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
    productionToTerminal :: [Maybe Integer],
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
    analyzerKind :: AnalyzerKind
  }
  deriving (Show)

toTerminalIndexMap :: (Ord t) => [t] -> Map t Integer
toTerminalIndexMap = Map.fromList . flip zip [0 ..]

toSymbolIndexMap ::
  (Ord t, Ord nt) =>
  Map t Integer ->
  [nt] ->
  Map (Symbol nt t) Integer
toSymbolIndexMap ts_map nts =
  Map.union (Map.mapKeys Terminal ts_map) nts_map
  where
    max_index = maximum ts_map
    nts_map = Map.fromList $ zip (map Nonterminal nts) [max_index ..]

mkTerminalType ::
  Map t Integer ->
  Either Text (Integer, UInt)
mkTerminalType ts_map =
  fmap (dead_token,) $
    maybeToEither err $
      toIntType dead_token
  where
    dead_token = succ $ maximum ts_map
    err = "Error: There are too many terminals to find a integral type."

mkProductionToTerminal ::
  (Ord nt, Ord t) =>
  Map (Symbol (AugmentedNonterminal (Either nt t)) (AugmentedTerminal t)) Integer ->
  [Production (AugmentedNonterminal (Either nt t)) (AugmentedTerminal t)] ->
  [Maybe Integer]
mkProductionToTerminal symbol_to_index prods =
  p . nonterminal <$> prods
  where
    p (AugmentedNonterminal (Right t)) = Just x
      where
        x = symbol_to_index Map.! Terminal (AugmentedTerminal t)
    p _ = Nothing

mkLexer :: CFG -> Either Text Analyzer
mkLexer cfg = do
  t_rules <- everyTRule cfg
  let terminal_map = toTerminalIndexMap (ruleT <$> t_rules)
  (dead_token, terminal_type) <- mkTerminalType terminal_map
  lexer <- cfgToDFALexer cfg
  parallel_lexer <- intDfaParallelLexer terminal_map dead_token lexer
  state_type <- extEndoType $ parLexer parallel_lexer
  transition_to_state <- transitionToStateArray parallel_lexer
  pure $
    Analyzer
      { analyzerKind =
          Lex $
            Lexer
              { stateType = state_type,
                lexer = parallel_lexer,
                deadToken = dead_token,
                transitionToState = transition_to_state,
                ignoreToken = Map.lookup (T "ignore") terminal_map
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
  let terminal_map = toTerminalIndexMap $ terminals grammar
      symbol_index_map = toSymbolIndexMap terminal_map (nonterminals grammar)
      production_to_terminal = mkProductionToTerminal symbol_index_map $ productions grammar
  (_, terminal_type) <- mkTerminalType terminal_map
  (start_terminal, end_terminal) <- startEndIndex symbol_index_map
  bracket_type <- findBracketIntType symbol_index_map
  production_type <- findProductionIntType grammar
  hash_table <- llpHashTable q k U64 terminal_type grammar symbol_index_map
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
                productionType = production_type,
                productionToTerminal = production_to_terminal,
                llpTable = hash_table,
                arities = mkArities grammar,
                numberOfTerminals = length $ terminals grammar,
                numberOfProductions = length $ productions grammar
              },
        terminalType = terminal_type
      }

unaugmentTerminalMap :: (Ord t) => Map (AugmentedTerminal t) a -> Map t a
unaugmentTerminalMap =
  Map.mapKeys unaug
    . Map.filterWithKey isTer
  where
    unaug (AugmentedTerminal a) = a
    unaug _ = error "This should not happened."
    isTer (AugmentedTerminal _) _ = True
    isTer _ _ = False

mkLexerParser :: Int -> Int -> CFG -> Either Text Analyzer
mkLexerParser q k cfg = do
  grammar <-
    augmentGrammar
      . extendByTerminals
      <$> cfgToGrammar cfg
  let terminal_map = toTerminalIndexMap (terminals grammar)
  (dead_token, terminal_type) <- mkTerminalType terminal_map
  let symbol_index_map = toSymbolIndexMap terminal_map (nonterminals grammar)
      production_to_terminal = mkProductionToTerminal symbol_index_map $ productions grammar
  (start_terminal, end_terminal) <- startEndIndex symbol_index_map
  bracket_type <- findBracketIntType symbol_index_map
  production_type <- findProductionIntType grammar
  hash_table <- llpHashTable q k U64 terminal_type grammar symbol_index_map
  lexer <- cfgToDFALexer cfg
  parallel_lexer <- intDfaParallelLexer (unaugmentTerminalMap terminal_map) dead_token lexer
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
                  bracketType = bracket_type,
                  productionType = production_type,
                  numberOfTerminals = length $ terminals grammar,
                  numberOfProductions = length $ productions grammar,
                  productionToTerminal = production_to_terminal,
                  llpTable = hash_table,
                  arities = mkArities grammar
                }
            ),
        terminalType = terminal_type
      }
