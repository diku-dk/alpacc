module Alpacc.RegularExpression (regExFromText, pRegEx) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1, string)
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

pLiteral :: Parser RegEx
pLiteral = Literal <$> lexeme (satisfy (`elem` ['a' .. 'z']))

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x =
      do
        f <- op
        y <- p
        rest (f x y)
        <|> return x

pConcat :: Parser RegEx
pConcat = foldl Concat Epsilon <$> many pTerm

pAlter :: Parser RegEx
pAlter = pConcat `chainl1` (lexeme (string "|") >> return Alter)

pRegEx :: Parser RegEx
pRegEx = pAlter

pTerm :: Parser RegEx
pTerm = do
  term <-
    choice
      [ pLiteral,
        between (lexeme "(") (lexeme ")") pRegEx
      ]
  s <- many (char '*')
  return $ if null s then term else Star term

regExFromText :: FilePath -> Text -> Either String RegEx
regExFromText fname s =
  either (Left . errorBundlePretty) Right $ parse (pRegEx <* eof) fname s