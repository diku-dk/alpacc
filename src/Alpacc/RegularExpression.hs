module Alpacc.RegularExpression (regExFromText, pRegEx) where

import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Text

data RegEx
  = Epsilon
  | Literal Char
  | Star RegEx
  | Alter RegEx RegEx
  | Concat RegEx RegEx
  deriving (Eq, Show)

space :: Parser ()
space = Lexer.space space1 (Lexer.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

symbol :: Text -> Parser Text
symbol = Lexer.symbol space

pLiteral :: Parser RegEx
pLiteral = Literal <$> lexeme (satisfy (`elem` ['a' .. 'z']))

binary :: Text -> (RegEx -> RegEx -> RegEx) -> Operator Parser RegEx
binary name f = InfixL (f <$ symbol name)

postfix :: Text -> (RegEx -> RegEx) -> Operator Parser RegEx
postfix name f = Postfix (f <$ symbol name)

operatorTable :: [[Operator Parser RegEx]]
operatorTable =
  [ [ postfix "*" Star
    ],
    [ binary "" Concat
    ],
    [ binary "|" Alter
    ]
  ]

pRegEx :: Parser RegEx
pRegEx = makeExprParser pTerm operatorTable

pTerm :: Parser RegEx
pTerm =
  choice
    [ between "(" ")" pRegEx,
      pLiteral
    ]

regExFromText :: FilePath -> Text -> Either String RegEx
regExFromText fname s =
  either (Left . errorBundlePretty) Right $ parse (pRegEx <* eof) fname s