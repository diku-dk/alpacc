module Alpacc.Lexer.RegularExpression
  ( regExFromText,
    pRegEx,
    RegEx (..),
    toWord8,
    producesEpsilon,
    printRegEx,
  )
where

import Codec.Binary.UTF8.String (encodeChar)
import Control.Monad (liftM2)
import Data.Char (chr, isDigit, isPrint, ord)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tuple
import Data.Void
import Data.Word (Word8)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, space1, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Text

data RegEx c
  = Epsilon
  | Literal c
  | Range [c]
  | Star (RegEx c)
  | Alter (RegEx c) (RegEx c)
  | Concat (RegEx c) (RegEx c)
  deriving (Eq, Ord, Show)

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
  [ ("\\", '\\'),
    (";", ';'),
    ("[", '['),
    ("]", ']'),
    ("(", '('),
    (")", ')'),
    ("*", '*'),
    ("+", '+'),
    ("-", '-'),
    ("|", '|'),
    ("s", ' '),
    ("t", '\t'),
    ("n", '\n'),
    ("r", '\r')
  ]

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
            [ try $
                (\a b -> [a .. b])
                  <$> pChar
                  <* lexeme "-"
                  <*> pChar,
              (: []) <$> pChar
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
  pPostfix term
  where
    pPostfix term =
      choice
        [ lexeme "*" *> pPostfix (Star term),
          lexeme "+" *> pPostfix (Concat term (Star term)),
          lexeme "?" *> pPostfix (Alter Epsilon term),
          pure term
        ]

regExFromText :: FilePath -> Text -> Either String (RegEx Char)
regExFromText fname s =
  either (Left . errorBundlePretty) Right $
    parse (pRegEx <* eof) fname s

toWord8 :: RegEx Char -> RegEx (NonEmpty Word8)
toWord8 = fmap (NonEmpty.fromList . encodeChar)

charToText :: Char -> Text
charToText c =
  case List.lookup c rev_escape of
    Just x -> "\\" <> x
    Nothing ->
      if isPrint' c
        then Text.pack [c]
        else ("\\" <>) $ Text.pack $ show $ ord c
  where
    rev_escape = swap <$> escapeChars

printRegEx :: RegEx Char -> Text
printRegEx Epsilon = ""
printRegEx (Literal c) = charToText c
printRegEx (Range cs) =
  ("[" <>) $
    (<> "]") $
      Text.intercalate " " $
        charToText <$> cs
printRegEx (Star r) = "(" <> printRegEx r <> ")*"
printRegEx (Concat r1 r2) =
  "(" <> printRegEx r1 <> " " <> printRegEx r2 <> ")"
printRegEx (Alter r1 r2) =
  "(" <> printRegEx r1 <> "|" <> printRegEx r2 <> ")"
