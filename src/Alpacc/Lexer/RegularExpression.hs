module Alpacc.Lexer.RegularExpression
  ( regExFromText,
    pRegEx,
    RegEx (..),
    toWord8,
    producesEpsilon
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

producesEpsilon :: RegEx c -> Bool
producesEpsilon Epsilon = True
producesEpsilon (Literal _) = False
producesEpsilon (Range []) = True
producesEpsilon (Range _) = False
producesEpsilon (Star _) = True
producesEpsilon (Alter a b) = producesEpsilon a || producesEpsilon b
producesEpsilon (Concat a b) = producesEpsilon a && producesEpsilon b

space :: Parser ()
space = Lexer.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

escapeChars :: [(Text, Char)]
escapeChars =
  [("\\",'\\')
  ,(";", ';')
  ,("[", '[')
  ,("]", ']')
  ,("(", '(')
  ,(")", ')')
  ,("*", '*')
  ,("+", '+')
  ,("-", '-')
  ,("|", '|')
  ,("s", ' ')
  ,("t", '\t')
  ,("n", '\n')
  ,("r", '\r')]

pEscapeChar :: Text -> Char -> Parser Char
pEscapeChar a b = do
  _ <- string a
  return b

pEscapeCharList :: [Parser Char]
pEscapeCharList = map (uncurry pEscapeChar) escapeChars

pEscapeChars :: Parser Char
pEscapeChars = lexeme $ do
  _ <- char '\\'
  choice pEscapeCharList

pUnicode :: Parser Char
pUnicode = fmap (chr . read . Text.unpack) . lexeme $ do
  _ <- char '\\'
  takeWhile1P Nothing isDigit

isPrint' :: Char -> Bool
isPrint' c = c `notElem` [';', '\\', '[', ']', '(', ')', '*', '|', '+', '-'] && isPrint c

pIsPrint :: Parser Char
pIsPrint = lexeme $ satisfy isPrint'

pChar :: Parser Char
pChar =
  choice
    [ try pEscapeChars,
      try pUnicode,
      pIsPrint
    ]

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
      if '*' `elem` postfixes
        then Star term
        else Concat term (Star term)
    Nothing -> term

regExFromText :: FilePath -> Text -> Either String (RegEx Char)
regExFromText fname s =
  either (Left . errorBundlePretty) Right
  $ parse (pRegEx <* eof) fname s

toWord8 :: RegEx Char -> RegEx [Word8]
toWord8 = fmap encodeChar
