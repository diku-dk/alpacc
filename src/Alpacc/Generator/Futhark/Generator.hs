module Alpacc.Generator.Futhark.Generator
  ( generate )
where

import Data.String.Interpolate (i)
import Alpacc.Generator.Futhark.Lexer
import Alpacc.Generator.Futhark.Parser
import Alpacc.CFG
import Alpacc.Grammar
import Data.Map qualified as Map
import Data.Map ( Map )
import Alpacc.Generator.Futhark.Util

parseFunction :: String
parseFunction = [i|
entry parse s =
  match lexer.lex s
  case #just s' -> parser.parse s'.0
  case #nothing -> []
|]

toTerminalIndexMap :: Ord t => Grammar nt t -> Map t Integer
toTerminalIndexMap = Map.fromList . flip zip [0..] . terminals

toSymbolIndexMap ::
  (Ord t, Ord nt) =>
  Grammar nt t ->
  Map (Symbol (AugmentedNonterminal nt) (AugmentedTerminal t)) Integer
toSymbolIndexMap grammar = Map.union aug_terminal_map nts_map
  where
    nts = nonterminals grammar
    terminal_map = Map.mapKeys AugmentedTerminal $ toTerminalIndexMap grammar
    max_index = maximum terminal_map
    new_terminals =
      Map.union terminal_map
      $ Map.fromList
        [(LeftTurnstile, max_index + 1)
        , (RightTurnstile, max_index + 2)]
    aug_terminal_map = Map.mapKeys Terminal new_terminals
    nts' = (++[Nonterminal Start]) $ Nonterminal . AugmentedNonterminal <$> nts
    nts_map = Map.fromList $ zip nts' [max_index+3..]

findTerminalIntegral ::
  Map (Symbol (AugmentedNonterminal NT) (AugmentedTerminal T)) Integer ->
  Either String FutUInt
findTerminalIntegral index_map = findSize _max
  where
    _max = maximum $ Map.filterWithKey (\k _ -> isTerminal k) index_map
    findSize max_size
      | max_size < 0 = Left "Max size may not be negative."
      | max_size < maxFutUInt U8 = Right U8
      | max_size < maxFutUInt U16 = Right U16
      | max_size < maxFutUInt U32 = Right U32
      | max_size < maxFutUInt U64 = Right U64
      | otherwise = Left "There are too many terminals to find a Futhark integral type."

generate :: Int -> Int -> CFG -> Either String String
generate q k cfg = do
  grammar <- cfgToGrammar cfg
  let terminal_index_map = toTerminalIndexMap grammar
  let symbol_index_map = toSymbolIndexMap grammar
  terminal_type <- findTerminalIntegral symbol_index_map
  dfa <- cfgToDFA cfg
  parser <- generateParser q k grammar symbol_index_map terminal_type
  lexer <- generateLexer dfa terminal_index_map terminal_type
  return $
    unlines
      [ parser
      , lexer
      , parseFunction
      ]
    