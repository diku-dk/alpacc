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
import Alpacc.Types
import Data.Either.Extra
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Word

mkLexer :: CFG -> Either Text (DFALexer Word8 Integer T)
mkLexer cfg = do
  spec <- cfgToDFALexerSpec cfg
  pure $ lexerDFA (0 :: Integer) spec
