module Alpacc.CFG
  ( cfgFromText,
    cfgToGrammar,
    cfgToRegEx,
    CFG (..),
    TRule (..),
    NTRule (..),
  )
where

import Data.Char (isAlphaNum, isLower, isPrint, isUpper)
import Data.List (nub)
import Data.Set qualified as Set hiding (Set)
import Data.Set (Set)
import Data.Text qualified as Text hiding (Text)
import Data.Text (Text)
import Data.Map qualified as Map hiding (Map)
import Data.Void
import Alpacc.Grammar
import Alpacc.RegularExpression
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Data.Foldable
-- import Debug.Trace (traceShow)
-- 
-- debug :: Show b => b -> b
-- debug x = traceShow x x

-- | Terminal formation rule.
data TRule = TRule
  { ruleT :: T,
    ruleRegex :: RegEx T
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

symbolTerminal :: Symbol NT T -> Set T
symbolTerminal (Terminal t) = Set.singleton t
symbolTerminal (Nonterminal _) = mempty

ruleTerminals :: NTRule -> Set T
ruleTerminals = foldMap (foldMap symbolTerminal) . ruleProductions

cfgToGrammar :: CFG -> Either String (Grammar NT T)
cfgToGrammar (CFG {ntRules = []}) = Left "CFG has no production rules."
cfgToGrammar (CFG {tRules, ntRules}) =
  let productions = concatMap ruleProds ntRules
      nonterminals = map ruleNT ntRules
      start = ruleNT $ head ntRules
      terminals = nub $ map ruleT tRules ++ toList (foldMap ruleTerminals ntRules)
   in Right $ Grammar {start, terminals, nonterminals, productions}
  where
    ruleProds NTRule {ruleNT, ruleProductions} =
      map (Production ruleNT) ruleProductions

implicitTRules :: CFG -> Either String [TRule]
implicitTRules (CFG {tRules, ntRules}) = mapM implicitLitToRegEx implicit
  where
    declared = map ruleT tRules
    implicit = filter (`notElem` declared) $ toList $ foldMap ruleTerminals ntRules
    implicitLitToRegEx t@(TLit s) = Right $ TRule {ruleT=t, ruleRegex=regex}
      where
        regex = foldl1 Concat $ fmap Literal (Text.unpack s)
    implicitLitToRegEx (T s) = Left $ "Can not create literal from: " <> Text.unpack s

tRuleToTuple :: TRule -> (T, RegEx T)
tRuleToTuple (TRule {ruleT=t, ruleRegex=regex}) = (t, regex)

cfgToRegEx :: CFG -> Either String (RegEx T)
cfgToRegEx cfg@(CFG {tRules}) = do
  implicit_t_rules <- implicitTRules cfg
  let all_t_rules = implicit_t_rules ++ tRules
  let terminal_map = Map.fromList $ tRuleToTuple <$> all_t_rules
  return $ mkTokenizerRegEx terminal_map

type Parser = Parsec Void Text

space :: Parser ()
space = Lexer.space space1 (Lexer.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

pNT :: Parser NT
pNT = lexeme (NT . Text.pack <$> p) <?> "nonterminal"
  where
    p = (:) <$> satisfy isUpper <*> many (satisfy ok)
    ok c = isAlphaNum c || c `elem` ("'*" :: String)

pStringLit :: Parser Text
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

pTRule :: Parser TRule
pTRule =
  TRule <$> pT <* lexeme "=" <*> pRegEx <* lexeme ";"
    <?> "terminal rule"

pNTRule :: Parser NTRule
pNTRule =
  NTRule <$> pNT <* lexeme "=" <*> (pRHS `sepBy` lexeme "|") <* lexeme ";"
    <?> "nonterminal rule"
  where
    pRHS = many pSymbol

pCFG :: Parser CFG
pCFG = CFG <$> many pTRule <*> many pNTRule

cfgFromText :: FilePath -> Text -> Either String CFG
cfgFromText fname s =
  either (Left . errorBundlePretty) Right $ parse (pCFG <* eof) fname s
