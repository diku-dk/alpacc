module ParallelParser.CFG
  ( cfgFromText,
    cfgToGrammar,
    CFG (..),
    TRule (..),
    NTRule (..),
  )
where

import Control.Monad (void)
import Data.Char (isAlphaNum, isDigit, isLower, isPrint, isUpper)
import Data.List (nub)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
import ParallelParser.Grammar
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- Extremely trivial; improve later.
data Regex
  = RegexSpanPlus Char Char -- [x-y]+
  | RegexConst T.Text
  deriving (Show)

-- | Terminal formation rule.
data TRule = TRule
  { ruleT :: T,
    ruleRegex :: Regex
  }
  deriving (Show)

-- | Nonterminal formation rule.
data NTRule = NTRule
  { ruleNT :: NT,
    ruleProductions :: [[Symbol NT T]]
  }
  deriving (Show)

data CFG = CFG
  { tRules :: [TRule],
    ntRules :: [NTRule]
  }
  deriving (Show)

cfgToGrammar :: CFG -> Either String (Grammar NT T)
cfgToGrammar (CFG {ntRules = []}) = Left "CFG has no production rules."
cfgToGrammar (CFG {tRules, ntRules}) =
  let productions = concatMap ruleProds ntRules
      nonterminals = map ruleNT ntRules
      start = ruleNT $ head ntRules
      terminals = nub $ map ruleT tRules ++ S.toList (foldMap ruleTerminals ntRules)
   in Right $ Grammar {start, terminals, nonterminals, productions}
  where
    symbolTerminal (Terminal t) = S.singleton t
    symbolTerminal (Nonterminal _) = mempty
    ruleProds NTRule {ruleNT, ruleProductions} =
      map (Production ruleNT) ruleProductions
    ruleTerminals = foldMap (foldMap symbolTerminal) . ruleProductions

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

pStringLit :: Parser T.Text
pStringLit = lexeme $ char '"' *> takeWhile1P Nothing ok <* char '"'
  where
    ok c = isPrint c && c /= '"'

pT :: Parser T
pT = T . T.unpack <$> p <?> "terminal"
  where
    p = lexeme $ takeWhile1P Nothing isLower

pTSym :: Parser T
pTSym =
  pT <|> (T . T.unpack <$> pStringLit) <?> "terminal"

pSymbol :: Parser (Symbol NT T)
pSymbol = Terminal <$> pTSym <|> Nonterminal <$> pNT

enclose :: Parser () -> Parser () -> Parser a -> Parser a
enclose pl pr x = pl *> x <* pr

brackets :: Parser a -> Parser a
brackets = enclose (void (lexeme "[")) (void (lexeme "]"))

pRegex :: Parser Regex
pRegex =
  brackets
    ( RegexSpanPlus
        <$> satisfy isAlphaNum
        <* lexeme "-"
        <*> satisfy isAlphaNum
    )
    <* lexeme "+"
    <|> RegexConst <$> pStringLit

pTRule :: Parser TRule
pTRule =
  TRule <$> pT <* lexeme "=" <*> pRegex <* lexeme ";"
    <?> "terminal rule"

pNTRule :: Parser NTRule
pNTRule =
  NTRule <$> pNT <* lexeme "=" <*> (pRHS `sepBy` lexeme "|") <* lexeme ";"
    <?> "nonterminal rule"
  where
    pRHS = many pSymbol

pCFG :: Parser CFG
pCFG = CFG <$> many pTRule <*> many pNTRule

cfgFromText :: FilePath -> T.Text -> Either String CFG
cfgFromText fname s =
  either (Left . errorBundlePretty) Right $ parse (pCFG <* eof) fname s
