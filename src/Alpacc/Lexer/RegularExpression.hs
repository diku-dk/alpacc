module Alpacc.Lexer.RegularExpression
  ( regExFromText,
    pRegEx,
    RegEx (..),
    producesEpsilon,
    genRegEx,
    Bytes (..),
    bytesToText,
    charToBytes,
  )
where

import Codec.Binary.UTF8.String (encodeChar)
import Control.Monad (liftM2)
import Data.Char (chr, digitToInt, isAlphaNum, isHexDigit, isPrint, isSpace)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Data.Word (Word8)
import Numeric (readHex)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Gen,
    choose,
    oneof,
    sized,
    vectorOf,
  )
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, space1, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Text.Printf (printf)

type Parser = Parsec Void Text

data RegEx c
  = Epsilon
  | Literal c
  | Range (NonEmpty c)
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
producesEpsilon (Range _) = False
producesEpsilon (Star _) = True
producesEpsilon (Alter a b) = producesEpsilon a || producesEpsilon b
producesEpsilon (Concat a b) = producesEpsilon a && producesEpsilon b

newtype Bytes = Bytes {unBytes :: NonEmpty Word8}
  deriving (Eq, Ord, Show)

instance Arbitrary Bytes where
  arbitrary = sized $ \i -> do
    s <- arbitrary
    vec <- vectorOf i arbitrary
    pure $ Bytes $ NonEmpty.fromList $ s : vec
  shrink =
    fmap (Bytes . NonEmpty.fromList)
      . filter (not . null)
      . shrink
      . NonEmpty.toList
      . unBytes

charToBytes :: Char -> Bytes
charToBytes = Bytes . NonEmpty.fromList . encodeChar

space :: Parser ()
space = Lexer.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

escapeChars :: [(Text, Char)]
escapeChars =
  [ ("\\", '\\'),
    ("/", '/'),
    ("[", '['),
    ("]", ']'),
    ("(", '('),
    (")", ')'),
    ("*", '*'),
    ("+", '+'),
    ("|", '|'),
    ("s", ' '),
    ("t", '\t'),
    ("n", '\n'),
    ("r", '\r'),
    ("?", '?')
  ]

pEscapeChar :: Text -> Char -> Parser Bytes
pEscapeChar a b = do
  _ <- string a
  return $ charToBytes b

pEscapeCharList :: [Parser Bytes]
pEscapeCharList = map (uncurry pEscapeChar) escapeChars

pEscapeChars :: Parser Bytes
pEscapeChars = lexeme $ do
  _ <- char '\\'
  choice pEscapeCharList

pUnicode :: Parser Bytes
pUnicode = lexeme $ do
  _ <- char '\\' <* char 'u'
  digits <- Text.unpack <$> takeWhile1P Nothing isHexDigit
  let (n, _) NonEmpty.:| _ = NonEmpty.fromList $ readHex digits
  return $ charToBytes $ chr n

pByte :: Parser Word8
pByte = lexeme $ do
  _ <- char '\\' <* char 'x'
  d1 <- satisfy isHexDigit
  d2 <- satisfy isHexDigit
  return $ fromIntegral (digitToInt d1 * 16 + digitToInt d2)

isPrint' :: Char -> Bool
isPrint' c =
  c `notElem` ['\\', '/', '[', ']', '(', ')', '*', '|', '+', '?']
    && isPrint c
    && not (isSpace c)

pIsPrint :: Parser Bytes
pIsPrint = lexeme $ charToBytes <$> satisfy isPrint'

pChar :: Parser Bytes
pChar =
  choice
    [ try pEscapeChars,
      try pUnicode,
      Bytes . NonEmpty.singleton <$> try pByte,
      pIsPrint
    ]

pLiteral :: Parser (RegEx Bytes)
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

pConcat :: Parser (RegEx Bytes)
pConcat = foldl Concat Epsilon <$> many pTerm

pAlter :: Parser (RegEx Bytes)
pAlter = pConcat `chainl1` (lexeme (string "|") >> return Alter)

pRegEx :: Parser (RegEx Bytes)
pRegEx = pAlter

pRange :: Parser (RegEx Bytes)
pRange =
  between (lexeme "[") (lexeme "]") $
    Range . NonEmpty.fromList . concat
      <$> many1
        ( choice
            [ try $
                (\a b -> charToBytes <$> [a .. b])
                  <$> pAlphaNum
                  <* lexeme "-"
                  <*> pAlphaNum,
              try $
                (\a b -> Bytes . NonEmpty.singleton <$> [a .. b])
                  <$> pByte
                  <* lexeme "-"
                  <*> pByte,
              try $
                (: []) . Bytes . NonEmpty.singleton <$> pByte,
              (: []) . charToBytes <$> pAlphaNum
            ]
        )
  where
    pAlphaNum = lexeme (satisfy isAlphaNum)

pTerm :: Parser (RegEx Bytes)
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

regExFromText :: FilePath -> Text -> Either String (RegEx Bytes)
regExFromText fname s =
  either (Left . errorBundlePretty) Right $
    parse (pRegEx <* eof) fname s

bytesToText :: Bytes -> Text
bytesToText = foldMap (Text.pack . printf "\\x%02x") . unBytes

pickN :: Int -> [a] -> Gen [a]
pickN n xs = do
  indices <- vectorOf n $ choose (0, length xs - 1)
  pure [xs !! j | j <- indices]

pickOne :: [a] -> Gen a
pickOne xs = do
  j <- choose (0, length xs - 1)
  pure $ xs !! j

genRegEx :: (Arbitrary c) => [c] -> Int -> Gen (RegEx c)
genRegEx cs size
  | size <= 1 =
      oneof
        [ pure Epsilon,
          Literal <$> pickOne cs,
          Range <$> nonEmptyArbitray
        ]
  | otherwise =
      oneof
        [ pure Epsilon,
          Literal <$> pickOne cs,
          Range <$> nonEmptyArbitray,
          Star <$> genRegEx cs (size `div` 2),
          Concat
            <$> genRegEx cs (size `div` 2)
            <*> genRegEx cs (size `div` 2),
          Alter
            <$> genRegEx cs (size `div` 2)
            <*> genRegEx cs (size `div` 2)
        ]
  where
    nonEmptyArbitray = do
      x <- pickOne cs
      xs <- pickN size cs
      pure $ NonEmpty.fromList $ x : xs

instance (Arbitrary c) => Arbitrary (RegEx c) where
  arbitrary = sized $ \i -> do
    vec <- vectorOf i arbitrary
    genRegEx vec i
  shrink Epsilon = []
  shrink (Literal c) = Epsilon : [Literal c' | c' <- shrink c]
  shrink (Range r) = Epsilon : [Range r' | r' <- shrinkNonEmpty r]
    where
      shrinkNonEmpty =
        fmap NonEmpty.fromList
          . filter (not . null)
          . shrink
          . NonEmpty.toList
  shrink (Star r) = [Epsilon, r] ++ [Star r' | r' <- shrink r]
  shrink (Concat r1 r2) =
    [r1, r2]
      ++ [Concat r1' r2 | r1' <- shrink r1]
      ++ [Concat r1 r2' | r2' <- shrink r2]
  shrink (Alter r1 r2) =
    [r1, r2]
      ++ [Alter r1' r2 | r1' <- shrink r1]
      ++ [Alter r1 r2' | r2' <- shrink r2]
