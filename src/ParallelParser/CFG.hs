module ParallelParser.CFG
  ( cfgFromText,
    cfgToGrammar,
    CFG (..),
    Rule (..),
  )
where

import Data.Char (isDigit, isLower, isPrint, isUpper)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
import ParallelParser.Grammar
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as Lexer

data Rule = Rule
  { ruleNT :: NT,
    ruleProductions :: [[Symbol NT T]]
  }

data CFG = CFG
  { cfgRules :: [Rule]
  }

cfgToGrammar :: CFG -> Either String (Grammar NT T)
cfgToGrammar (CFG {cfgRules = []}) = Left "CFG has no production rules."
cfgToGrammar (CFG {cfgRules}) =
  let productions = concatMap ruleProds cfgRules
      nonterminals = map ruleNT cfgRules
      start = ruleNT $ head cfgRules
      terminals = S.toList $ foldMap (foldMap (foldMap symbolTerminal) . ruleProductions) cfgRules
   in Right $ Grammar {start, terminals, nonterminals, productions}
  where
    symbolTerminal (Terminal t) = S.singleton t
    symbolTerminal (Nonterminal _) = mempty
    ruleProds Rule {ruleNT, ruleProductions} =
      map (Production ruleNT) ruleProductions

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
    pLit = char '"' *> takeWhile1P (Just "terminal constituent") constituent <* char '"'
    p = takeWhile1P Nothing isLower
    constituent c = isPrint c && c /= '"'

pSymbol :: Parser (Symbol NT T)
pSymbol = Terminal <$> pT <|> Nonterminal <$> pNT

pRule :: Parser Rule
pRule =
  Rule <$> pNT <* lexeme "=" <*> (pRHS `sepBy` lexeme "|") <* lexeme ";"
    <?> "production rule"
  where
    pRHS = many pSymbol

pCFG :: Parser CFG
pCFG = CFG <$> many pRule

cfgFromText :: FilePath -> T.Text -> Either String CFG
cfgFromText fname s =
  either (Left . errorBundlePretty) Right $ parse (pCFG <* eof) fname s
