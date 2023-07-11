module CFG (grammarFromText) where

import Data.Char (isDigit, isLower, isPrint, isUpper)
import Data.List qualified as L
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
import ParallelParser.Grammar
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void T.Text

space :: Parser ()
space = Lexer.space space1 (Lexer.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

pNT :: Parser NT
pNT = lexeme (NT <$> p) <?> "nonterminal"
  where
    p = (:) <$> satisfy isUpper <*> many (satisfy ok)
    ok c = isUpper c || isDigit c

pT :: Parser T
pT =
  lexeme (T . T.unpack <$> (pLit <|> p)) <?> "terminal"
  where
    pLit = char '\'' *> takeWhile1P (Just "terminal constituent") constituent <* char '\''
    p = takeWhile1P Nothing isLower
    constituent c = isPrint c && c /= '\''

pSymbol :: Parser (Symbol NT T)
pSymbol = Terminal <$> pT <|> Nonterminal <$> pNT

pProduction :: Parser (Production NT T)
pProduction =
  Production <$> pNT <* lexeme "->" <*> many pSymbol <* lexeme ";"
    <?> "production"

pGrammar :: Parser (Grammar NT T)
pGrammar = build <$> pProduction <*> many pProduction
  where
    build p ps =
      let productions = p : ps
          nonterminals = L.nub $ map prodLHS productions
          start = prodLHS p
          terminals = S.toList $ foldMap (foldMap symbolTerminal . prodRHS) productions
       in Grammar {start, terminals, nonterminals, productions}
    symbolTerminal (Terminal t) = S.singleton t
    symbolTerminal (Nonterminal _) = mempty

grammarFromText :: FilePath -> T.Text -> Either String (Grammar NT T)
grammarFromText fname s =
  either (Left . errorBundlePretty) Right $ parse (pGrammar <* eof) fname s
