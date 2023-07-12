module ParallelParser.CFG
  ( cfgFromText,
    cfgToGrammar,
    cfgToLexer,
    CFG (..),
    TRule (..),
    NTRule (..),
  )
where

import Control.Monad (void)
import Data.Char (isAlphaNum, isLower, isPrint, isUpper)
import Data.List (nub, nubBy)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
import ParallelParser.Grammar
import ParallelParser.Lexer
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- Extremely trivial; improve later.
data Regex
  = RegexSpanPlus [(Char, Char)] -- [x-y]+
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

symbolTerminal :: Symbol NT T -> S.Set T
symbolTerminal (Terminal t) = S.singleton t
symbolTerminal (Nonterminal _) = mempty

ruleTerminals :: NTRule -> S.Set T
ruleTerminals = foldMap (foldMap symbolTerminal) . ruleProductions

cfgToGrammar :: CFG -> Either String (Grammar NT T)
cfgToGrammar (CFG {ntRules = []}) = Left "CFG has no production rules."
cfgToGrammar (CFG {tRules, ntRules}) =
  let productions = concatMap ruleProds ntRules
      nonterminals = map ruleNT ntRules
      start = ruleNT $ head ntRules
      terminals = nub $ map ruleT tRules ++ S.toList (foldMap ruleTerminals ntRules)
   in Right $ Grammar {start, terminals, nonterminals, productions}
  where
    ruleProds NTRule {ruleNT, ruleProductions} =
      map (Production ruleNT) ruleProductions

cfgToLexer :: CFG -> Either String [LexerRule]
cfgToLexer (CFG {tRules, ntRules}) =
  mapM rule $ nubBy sameT $ tRules <> map mkImplicitRule implicit
  where
    sameT x y = ruleT x == ruleT y
    declared = map ruleT tRules
    implicit = filter (`notElem` declared) $ S.toList $ foldMap ruleTerminals ntRules
    rule (TRule {ruleRegex = RegexConst s})
      | [c] <- T.unpack s =
          Right $ LChar c
    rule (TRule {ruleRegex = RegexSpanPlus xys}) =
      Right $ LChars xys
    rule (TRule {ruleT = T s}) =
      Left $ "Cannot handle rule for terminal " <> T.unpack s
    rule (TRule {ruleT = TLit s}) =
      Left $ "Cannot handle rule for terminal " <> T.unpack s
    mkImplicitRule (T t) = TRule {ruleT = T t, ruleRegex = RegexConst t}
    mkImplicitRule (TLit t) = TRule {ruleT = TLit t, ruleRegex = RegexConst t}

type Parser = Parsec Void T.Text

space :: Parser ()
space = Lexer.space space1 (Lexer.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

pNT :: Parser NT
pNT = lexeme (NT . T.pack <$> p) <?> "nonterminal"
  where
    p = (:) <$> satisfy isUpper <*> many (satisfy ok)
    ok c = isAlphaNum c || c `elem` ("'*" :: String)

pStringLit :: Parser T.Text
pStringLit = lexeme $ char '"' *> takeWhile1P Nothing ok <* char '"'
  where
    ok c = isPrint c && c /= '"'

pT :: Parser T
pT = T <$> p <?> "terminal"
  where
    p = lexeme $ takeWhile1P Nothing isLower

pTSym :: Parser T
pTSym =
  pT <|> (TLit <$> pStringLit) <?> "terminal"

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
        <$> many
          ( (,)
              <$> satisfy isAlphaNum
              <* lexeme "-"
              <*> satisfy isAlphaNum
          )
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
