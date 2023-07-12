module ParallelParser.Lexer
  ( LexerRule (..),
    generateLexer,
  )
where

import Data.Char (ord)
import Data.String.Interpolate (i)

data LexerRule
  = LChar Char -- "c"
  | LChars [(Char, Char)] -- [x-y]+

charMatches :: LexerRule -> String
charMatches (LChar c) = [i|c == #{ord c}|]
charMatches (LChars []) = "false"
charMatches (LChars [(x, y)]) =
  [i|c >= #{ord x} && c <= #{ord y}|]
charMatches (LChars ((x, y) : xys)) =
  [i|c >= #{ord x} && c <= #{ord y} || #{charMatches (LChars xys)}|]

mapFunction :: [LexerRule] -> String
mapFunction rules =
  [i|def char_code (c: u8) : u32 =
       #{branches (zip [(0::Int)..] rules)}|]
  where
    branches [] = "if c == ' ' then whitespace else lexer_error"
    branches ((l, r) : rs) =
      [i|if #{charMatches r} then #{l} else
         #{branches rs}|]

acceptFunction :: [LexerRule] -> String
acceptFunction rules =
  [i|def accept (this: u32) (next: u32) =
       #{branches (zip [(0::Int)..] rules)}|]
  where
    branches [] = "if this == lexer_error then lexer_error else whitespace"
    branches ((l, LChar {}) : rs) =
      [i|if this == #{l} then this
         else #{branches rs}|]
    branches ((l, LChars {}) : rs) =
      [i|if this == #{l} && next != #{l} then this
         else #{branches rs}|]

-- | The rules must currently be be completely disjoint.  The order in
-- which they occur determines which integer is associated with each
-- lexeme.
generateLexer :: [LexerRule] -> String
generateLexer rules =
  unlines
    [ mapFunction rules,
      acceptFunction rules
    ]
