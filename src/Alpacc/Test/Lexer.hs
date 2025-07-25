module Alpacc.Test.Lexer
  ( mkLexer,
  )
where

import Alpacc.CFG
import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Data.Text (Text)
import Data.Word

mkLexer :: CFG -> Either Text (DFALexer Word8 Integer T)
mkLexer cfg = do
  spec <- dfaCharToWord8 <$> cfgToDFALexerSpec cfg
  pure $ lexerDFA (0 :: Integer) spec

-- properties :: [(String, Property)]
-- properties =
--   [("parsePrinted", property)]
