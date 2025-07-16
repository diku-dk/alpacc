module Alpacc.Encode
  ( SymbolEncoder,
    TerminalEncoder,
    Encoder,
  )
where

import Alpacc.Grammar
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe

class Encoder ctx where
  encode :: (Ord t) => ctx t -> t -> Maybe Integer

newtype SymbolEncoder nt t
  = SymbolEncoder
  { symbolEncoder :: Map (Symbol (AugmentedNonterminal nt) (AugmentedTerminal (Unused t))) Integer
  }

newtype TerminalEncoder t
  = TerminalEncoder
  { terminalEncoder :: Map (Unused t) Integer
  }

fromSymbolToTerminalEncoder :: (Ord t) => SymbolEncoder nt t -> TerminalEncoder t
fromSymbolToTerminalEncoder =
  TerminalEncoder
    . Map.mapKeys (fromMaybe (error "This will never happen."))
    . Map.filterWithKey (\k _ -> isJust k)
    . Map.mapKeys toTerminal
    . symbolEncoder
  where
    toTerminal (Terminal (AugmentedTerminal t)) = Just t
    toTerminal _ = Nothing
