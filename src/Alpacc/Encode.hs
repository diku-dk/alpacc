module Alpacc.Encode
  ( SymbolEncoder,
    TerminalEncoder,
    fromSymbolToTerminalEncoder,
    encodeSymbols,
    encodeTerminals,
    terminalLookup,
    symbolLookup,
    terminalMax,
    symbolMax,
    bracketIntType,
    productionIntType,
    numTerminals,
    numSymbols,
    symbolTerminalIntType,
    terminalIntType,
    terminalDead,
    symbolDead,
    symbolStartTerminal,
    symbolEndTerminal,
  )
where

import Alpacc.Grammar
import Alpacc.Types
import Data.Either.Extra
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text (Text)

newtype SymbolEncoder nt t
  = SymbolEncoder
  { symbolEncoder ::
      Map
        ( Symbol
            (AugmentedNonterminal (Symbol nt t))
            (AugmentedTerminal (Unused t))
        )
        Integer
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

findSingleton :: (Eq t) => t -> [t] -> [t]
findSingleton t = maybeToList . List.find (t ==)

encodeSymbols :: (Ord nt, Ord t) => t -> ParsingGrammar nt t -> SymbolEncoder nt t
encodeSymbols t_ignore grammar =
  SymbolEncoder $
    Map.fromList $
      zip s [0 ..]
  where
    ts' = terminals $ getGrammar grammar
    nts = nonterminals $ getGrammar grammar

    (tts, fts) = List.partition p ts'

    ts = fts ++ ignore ++ unused ++ right ++ left

    s = (Terminal <$> ts) ++ (Nonterminal <$> nts)

    left = findSingleton LeftTurnstile tts
    right = findSingleton RightTurnstile tts
    unused = findSingleton (AugmentedTerminal Unused) tts
    ignore = findSingleton (AugmentedTerminal (Used t_ignore)) tts

    p t =
      t == LeftTurnstile
        || t == RightTurnstile
        || AugmentedTerminal Unused == t
        || AugmentedTerminal (Used t_ignore) == t

encodeTerminals :: (Ord t) => t -> ParsingTerminals t -> TerminalEncoder t
encodeTerminals t_ignore ters =
  TerminalEncoder $
    Map.fromList $
      zip s [0 ..]
  where
    ts' = getTerminals ters
    (tts, fts) = List.partition p ts'
    unused = findSingleton Unused tts
    ignore = findSingleton (Used t_ignore) tts
    s = fts ++ ignore ++ unused

    p t =
      Unused == t
        || Used t_ignore == t

terminalLookup :: (Ord t) => t -> TerminalEncoder t -> Maybe Integer
terminalLookup t encoder =
  Map.lookup (Used t) (terminalEncoder encoder)

terminalDead :: (Ord t) => TerminalEncoder t -> Integer
terminalDead encoder =
  fromMaybe (error "This will not happen.") $
    Map.lookup Unused (terminalEncoder encoder)

symbolDead :: (Ord nt, Ord t) => SymbolEncoder nt t -> Integer
symbolDead encoder =
  fromMaybe (error "This will not happen.") $
    Map.lookup dead (symbolEncoder encoder)
  where
    dead = Terminal $ AugmentedTerminal Unused

symbolEndTerminal :: (Ord nt, Ord t) => SymbolEncoder nt t -> Integer
symbolEndTerminal encoder =
  fromMaybe (error "This will not happen.") $
    Map.lookup dead (symbolEncoder encoder)
  where
    dead = Terminal $ LeftTurnstile

symbolStartTerminal :: (Ord nt, Ord t) => SymbolEncoder nt t -> Integer
symbolStartTerminal encoder =
  fromMaybe (error "This will not happen.") $
    Map.lookup dead (symbolEncoder encoder)
  where
    dead = Terminal $ LeftTurnstile

symbolLookup ::
  (Ord t, Ord nt) =>
  Symbol
    (AugmentedNonterminal (Symbol nt t))
    (AugmentedTerminal (Unused t)) ->
  SymbolEncoder nt t ->
  Maybe Integer
symbolLookup s encoder =
  Map.lookup s (symbolEncoder encoder)

symbolMax :: SymbolEncoder nt t -> Integer
symbolMax = maximum . symbolEncoder

terminalMax :: TerminalEncoder t -> Integer
terminalMax = maximum . terminalEncoder

numTerminals :: TerminalEncoder t -> Int
numTerminals = Map.size . terminalEncoder

numTerminalSymbols :: SymbolEncoder nt t -> Int
numTerminalSymbols =
  Map.size
    . Map.filterWithKey (\k _ -> isTerminal k)
    . symbolEncoder

numSymbols :: TerminalEncoder t -> Int
numSymbols = Map.size . terminalEncoder

bracketIntType ::
  SymbolEncoder nt t ->
  Either Text UInt
bracketIntType =
  maybeToEither err
    . toIntType
    . (2 *)
    . symbolMax
  where
    err = "Error: There are too many terminals and none terminals to find a integral type for the bracket."

productionIntType ::
  ParsingGrammar nt t ->
  Either Text UInt
productionIntType =
  maybeToEither err
    . toIntType
    . fromIntegral
    . length
    . productions
    . getGrammar
  where
    err = "Error: There are too many productions to find a integral type."

symbolTerminalIntType :: SymbolEncoder nt t -> Either Text UInt
symbolTerminalIntType =
  maybeToEither err
    . toIntType
    . maximum
    . Map.filterWithKey (\k _ -> isTerminal k)
    . symbolEncoder
  where
    err = "Error: There are too many symbols to find a integral type."

terminalIntType :: TerminalEncoder t -> Either Text UInt
terminalIntType =
  maybeToEither err
    . toIntType
    . maximum
    . terminalEncoder
  where
    err = "Error: There are too many terminals to find a integral type."
