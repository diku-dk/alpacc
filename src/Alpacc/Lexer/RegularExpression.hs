module Alpacc.Lexer.RegularExpression
  ( regExFromText,
    pRegEx,
    RegEx (..),
    toWord8
  )
where

import Control.Monad.State
import Data.Char (chr, isDigit, isPrint)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, space1, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Codec.Binary.UTF8.String (encodeChar)
import Data.Word (Word8)

type Parser = Parsec Void Text

data RegEx c
  = Epsilon
  | Literal c
  | Range [c]
  | Star (RegEx c)
  | Alter (RegEx c) (RegEx c)
  | Concat (RegEx c) (RegEx c)
  deriving (Eq, Show)

instance Functor RegEx where
  fmap _ Epsilon = Epsilon
  fmap f (Literal c) = Literal (f c)
  fmap f (Range cs) = Range (fmap f cs)
  fmap f (Star r) = Star (fmap f r)
  fmap f (Alter r r') = Alter (fmap f r) (fmap f r')
  fmap f (Concat r r') = Concat (fmap f r) (fmap f r')

space :: Parser ()
space = Lexer.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

pBackslash :: Parser Char
pBackslash = Text.last <$> lexeme (string "\\\\")

pSemicolon :: Parser Char
pSemicolon = Text.last <$> lexeme (string "\\;")

pLeftSquareBracket :: Parser Char
pLeftSquareBracket = Text.last <$> lexeme (string "\\[")

pRightSquareBracket :: Parser Char
pRightSquareBracket = Text.last <$> lexeme (string "\\]")

pLeftParentheses :: Parser Char
pLeftParentheses = Text.last <$> lexeme (string "\\(")

pRightParentheses :: Parser Char
pRightParentheses = Text.last <$> lexeme (string "\\)")

pStar :: Parser Char
pStar = Text.last <$> lexeme (string "\\*")

pAdd :: Parser Char
pAdd = Text.last <$> lexeme (string "\\+")

pDash :: Parser Char
pDash = Text.last <$> lexeme (string "\\-")

pPipe :: Parser Char
pPipe = Text.last <$> lexeme (string "\\|")

pSpace :: Parser Char
pSpace = do
  _ <- lexeme $ string "\\s"
  return ' '

pTab :: Parser Char
pTab = do
  _ <- lexeme $ string "\\t"
  return '\t'

pNewline :: Parser Char
pNewline = do
  _ <- lexeme $ string "\\n"
  return '\n'

pCarriageReturn :: Parser Char
pCarriageReturn = do
  _ <- lexeme $ string "\\r"
  return '\r'

pUnicode :: Parser Char
pUnicode = fmap (chr . read . Text.unpack) . lexeme $ do
  _ <- string "\\"
  takeWhile1P Nothing isDigit

isPrint' :: Char -> Bool
isPrint' c = c `notElem` [';', '\\', '[', ']', '(', ')', '*', '|', '+', '-'] && isPrint c

pIsPrint :: Parser Char
pIsPrint = lexeme $ satisfy isPrint'

pChar :: Parser Char
pChar =
  choice
    [ try pBackslash,
      try pSemicolon,
      try pLeftSquareBracket,
      try pRightSquareBracket,
      try pLeftParentheses,
      try pRightParentheses,
      try pSpace,
      try pTab,
      try pNewline,
      try pCarriageReturn,
      try pStar,
      try pAdd,
      try pPipe,
      try pDash,
      try pUnicode,
      pIsPrint
    ]

-- satisfy isPrint

pLiteral :: Parser (RegEx Char)
pLiteral = Literal <$> lexeme pChar

many1 :: Parser a -> Parser [a]
many1 p = liftM2 (:) p (many p)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x =
      do
        f <- op
        y <- p
        rest (f x y)
        <|> return x

pConcat :: Parser (RegEx Char)
pConcat = foldl Concat Epsilon <$> many pTerm

pAlter :: Parser (RegEx Char)
pAlter = pConcat `chainl1` (lexeme (string "|") >> return Alter)

pRegEx :: Parser (RegEx Char)
pRegEx = pAlter

pRange :: Parser (RegEx Char)
pRange =
  between (lexeme "[") (lexeme "]") $
    Range . concat
      <$> many1
        ( choice
            [ try $ (\a b -> [a..b])
                    <$> pChar
                    <* lexeme "-"
                    <*> pChar
                ,
              (:[]) <$> pChar
            ]
        )

pTerm :: Parser (RegEx Char)
pTerm = do
  term <-
    choice
      [ pRange,
        pLiteral,
        between (lexeme "(") (lexeme ")") pRegEx
      ]
  s <- optional (many1 (char '*' <|> char '+'))
  return $ case s of
    -- I did a derivation and found (s*)+ = (s+)* = s* so it should hold if *
    -- occurs in a sequence of applied postfix operation then it will equal s*.
    -- If only + occurs in the postfix sequence then then due to (s+)+ = s+ it
    -- will simply correspond to ss*.
    Just postfixes ->
      if any (`elem` ['*']) postfixes
        then Star term
        else Concat term (Star term)
    Nothing -> term

regExFromText :: FilePath -> Text -> Either String (RegEx Char)
regExFromText fname s =
  either (Left . errorBundlePretty) Right $ parse (pRegEx <* eof) fname s

toWord8 :: RegEx Char -> RegEx [Word8]
toWord8 = fmap encodeChar