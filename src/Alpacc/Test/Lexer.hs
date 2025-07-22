module Alpacc.Test.Lexer
  (
  )
where

import Alpacc.CFG
import Alpacc.Encode
import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Alpacc.Lexer.DFAParallelLexer
import Alpacc.Lexer.Encode
import Alpacc.Lexer.ParallelLexing
import Alpacc.Lexer.RegularExpression
import Alpacc.Types
import Control.Monad
import Data.Char
import Data.Either.Extra
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Gen,
    Property,
    elements,
    generate,
    listOf,
    oneof,
    property,
    sized,
  )

mkLexer :: CFG -> Either Text (DFALexer Word8 Integer T)
mkLexer cfg = do
  spec <- dfaCharToWord8 <$> cfgToDFALexerSpec cfg
  pure $ lexerDFA (0 :: Integer) spec

-- properties :: [(String, Property)]
-- properties =
--   [("parsePrinted", property)]
