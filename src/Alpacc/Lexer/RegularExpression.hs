module Alpacc.Lexer.RegularExpression
  ( regExFromText,
    pRegEx,
    RegEx (..),
    mkTokenizerRegEx
  )
where

import Control.Monad.State
import Data.Composition
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, space1, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)

type Parser = Parsec Void Text

data RegEx t
  = Epsilon
  | Literal Char
  | Range [Either Char (Char, Char)]
  | Star (RegEx t)
  | Alter (RegEx t) (RegEx t)
  | Concat (RegEx t) (RegEx t)
  | Token t (RegEx t)
  deriving (Eq, Show)

space :: Parser ()
space = Lexer.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

validLiterials :: Set Char -- missing chars ";\[]()*|+-"
validLiterials = Set.fromList $ ['!'..'\''] ++ [','] ++ ['.'..':'] ++ ['<'..'Z'] ++ ['^'..'{'] ++ ['}'..'~']

pBackslash :: Parser Char
pBackslash = Text.last <$> string "\\\\"

pSemicolon :: Parser Char
pSemicolon = Text.last <$> string "\\;"

pLeftSquareBracket :: Parser Char
pLeftSquareBracket = Text.last <$> string "\\["

pRightSquareBracket :: Parser Char
pRightSquareBracket = Text.last <$> string "\\]"

pLeftParentheses :: Parser Char
pLeftParentheses = Text.last <$> string "\\("

pRightParentheses :: Parser Char
pRightParentheses = Text.last <$> string "\\)"

pStar :: Parser Char
pStar = Text.last <$> string "\\*"

pAdd :: Parser Char
pAdd = Text.last <$> string "\\+"

pDash :: Parser Char
pDash = Text.last <$> string "\\-"

pPipe :: Parser Char
pPipe = Text.last <$> string "\\|"

pSpace :: Parser Char
pSpace = do
  _ <- string "\\s"
  return ' '

pTab :: Parser Char
pTab = do
  _ <- string "\\t"
  return '\t'

pNewline :: Parser Char
pNewline = do
  _ <- string "\\n"
  return '\n'

pCarriageReturn :: Parser Char
pCarriageReturn = do
  _ <- string "\\r"
  return '\r'

pChar :: Parser Char
pChar =
  pBackslash <|>
  pSemicolon <|>
  pLeftSquareBracket <|>
  pRightSquareBracket <|>
  pLeftParentheses <|>
  pRightParentheses <|>
  pSpace <|>
  pTab <|>
  pNewline <|>
  pCarriageReturn <|>
  pStar <|>
  pAdd <|>
  pPipe <|>
  pDash <|>
  satisfy (`elem` validLiterials)

pLiteral :: Parser (RegEx t)
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

pConcat :: Parser (RegEx t)
pConcat = foldl Concat Epsilon <$> many pTerm

pAlter :: Parser (RegEx t)
pAlter = pConcat `chainl1` (lexeme (string "|") >> return Alter)

pRegEx :: Parser (RegEx t)
pRegEx = pAlter

pRange :: Parser (RegEx t)
pRange =
  between (lexeme "[") (lexeme "]") $
    Range
      <$> many1
        ( Right
            .: (,)
            <$> pChar
            <* lexeme "-"
            <*> pChar
            <|> Left
            <$> pChar
        )

pTerm :: Parser (RegEx t)
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

regExFromText :: FilePath -> Text -> Either String (RegEx t)
regExFromText fname s =
  either (Left . errorBundlePretty) Right $ parse (pRegEx <* eof) fname s

mkTokenizerRegEx :: Map t (RegEx t) -> RegEx t
mkTokenizerRegEx regex_map =
  if null regex_map
    then Epsilon
    else Star $ foldr1 Alter $ uncurry Token <$> Map.toList regex_map