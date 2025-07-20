module Alpacc.CFG
  ( cfgFromText,
    cfgToGrammar,
    cfgToDFALexerSpec,
    CFG (..),
    TRule (..),
    NTRule (..),
    everyTRule,
    dfaFromCfgFile,
  )
where

import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Alpacc.Lexer.RegularExpression
import Data.Char (isAlphaNum, isLower, isPrint, isUpper)
import Data.Foldable
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty hiding (NonEmpty)
import Data.Map qualified as Map hiding (Map)
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text hiding (Text)
import Data.Void
import Data.Word (Word8)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- | Terminal formation rule.
data TRule = TRule
  { ruleT :: T,
    ruleRegex :: RegEx Char
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

cfgToGrammar ::
  CFG ->
  Either Text (ParsingGrammar NT T)
cfgToGrammar (CFG {ntRules = []}) = Left "CFG has no production rules."
cfgToGrammar (CFG {tRules, ntRules}) =
  let productions = concatMap ruleProds ntRules
      nonterminals = map ruleNT ntRules
      start = ruleNT $ head ntRules
      terminals' = List.nub $ map ruleT tRules ++ toList (foldMap ruleTerminals ntRules)
      (a, b) = List.partition pred' terminals'
      terminals = a ++ b
      grammar = Grammar {start, terminals, nonterminals, productions}
   in case grammarError grammar of
        Just err -> Left err
        Nothing ->
          Right $ parsingGrammar grammar
  where
    pred' (T "ignore") = True
    pred' _ = False
    ruleProds NTRule {ruleNT, ruleProductions} =
      map (Production ruleNT) ruleProductions

everyTRule :: CFG -> Either Text [TRule]
everyTRule cfg@(CFG {tRules}) = do
  implicit_t_rules <- implicitTRules cfg
  return $ tRules ++ implicit_t_rules

implicitTRules :: CFG -> Either Text [TRule]
implicitTRules (CFG {tRules, ntRules}) = mapM implicitLitToRegEx implicit
  where
    declared = map ruleT tRules
    implicit = filter (`notElem` declared) $ toList $ foldMap ruleTerminals ntRules
    implicitLitToRegEx t@(TLit s) = Right $ TRule {ruleT = t, ruleRegex = regex}
      where
        regex = foldl1 Concat $ fmap Literal (Text.unpack s)
    implicitLitToRegEx (T s) = Left $ "Can not create literal from: " <> s

tRuleToTuple :: TRule -> (T, RegEx (NonEmpty Word8))
tRuleToTuple (TRule {ruleT = t, ruleRegex = regex}) = (t, NonEmpty.fromList <$> toWord8 regex)

cfgToDFALexerSpec :: CFG -> Either Text (DFALexerSpec Word8 Int T)
cfgToDFALexerSpec (CFG {tRules = []}) = Left "CFG has no lexical rules."
cfgToDFALexerSpec cfg@(CFG {tRules}) = do
  implicit_t_rules <- implicitTRules cfg
  let all_t_rules = implicit_t_rules ++ tRules
      t_rule_tuples = tRuleToTuple <$> all_t_rules
      x = find (producesEpsilon . snd) t_rule_tuples
  case x of
    Just (t, _) -> Left $ Text.pack [i|Error: #{t} may not produce empty strings.|]
    Nothing -> Right $ dfaLexerSpec 0 t_rule_tuples

type Parser = Parsec Void Text

space :: Parser ()
space = Lexer.space space1 (Lexer.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

pNT :: Parser NT
pNT = lexeme (NT . Text.pack <$> p) <?> "nonterminal"
  where
    p = (:) <$> satisfy isUpper <*> many (satisfy ok)
    ok c = isAlphaNum c || c `elem` ['\'', '*', '_', '"']

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

cfgFromText :: FilePath -> Text -> Either Text CFG
cfgFromText fname s =
  either (Left . Text.pack . errorBundlePretty) Right $ parse (pCFG <* eof) fname s

eitherToIO :: Either Text a -> IO a
eitherToIO (Right x) = return x
eitherToIO (Left err) = do
  hPutStrLn stderr (Text.unpack $ "Error: " <> err)
  exitFailure

dfaFromCfgFile :: FilePath -> IO (DFALexer Word8 Integer T)
dfaFromCfgFile path = do
  content <- Text.pack <$> readFile path
  cfg <- eitherToIO $ cfgFromText path content
  spec <- eitherToIO $ cfgToDFALexerSpec cfg
  pure $ lexerDFA (0 :: Integer) spec
