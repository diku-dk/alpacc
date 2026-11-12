module Alpacc.CFG
  ( cfgFromText,
    cfgToGrammar,
    cfgToDFALexerSpec,
    CFG (..),
    Params (..),
    printDfaSpec,
    properties,
    printGrammar,
  )
where

import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Alpacc.Lexer.RegularExpression
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isPrint)
import Data.Foldable
import Data.IntMap qualified as IntMap hiding (IntMap)
import Data.List qualified as List
import Data.Map qualified as Map hiding (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set hiding (Set)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text hiding (Text)
import Data.Void
import Test.QuickCheck
  ( Property,
    property,
  )
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- | Terminal formation rule.
data TRule = TRule
  { ruleT :: T,
    ruleRegex :: RegEx Bytes
  }
  deriving (Show)

-- | Nonterminal formation rule.
data NTRule = NTRule
  { ruleNT :: NT,
    ruleName :: Maybe Text,
    ruleProductions :: [Symbol NT T]
  }
  deriving (Show)

data Params = Params
  { paramsLookback :: Int,
    paramsLookahead :: Int,
    paramsLength :: Maybe Int  -- token length bits: 8, 16, 32, or 64; Nothing = same as index_t
  }
  deriving (Show)

defaultParams :: Params
defaultParams =
  Params
    { paramsLookback = 0,
      paramsLookahead = 1,
      paramsLength = Nothing
    }

data CFG = CFG
  { cfgParams :: Params,
    tRules :: [TRule],
    ntRules :: [NTRule]
  }
  deriving (Show)

pParam :: Parser (Params -> Params)
pParam = pLookback <|> pLookahead <|> pLength <?> "parameter assignment"
  where
    pParamInt name =
      lexeme name
        *> lexeme "="
        *> lexeme Lexer.decimal
        <* lexeme "."

    pLookback =
      (\n p -> p {paramsLookback = n}) <$> pParamInt "lookback"

    pLookahead =
      (\n p -> p {paramsLookahead = n}) <$> pParamInt "lookahead"

    pLength = do
      n <- pParamInt "length"
      if n `elem` ([8, 16, 32, 64] :: [Int])
        then pure (\p -> p {paramsLength = Just n})
        else fail "length must be 8, 16, 32, or 64"

pParams :: Parser Params
pParams =
  fmap (fromMaybe defaultParams)
    . optional
    $ lexeme "params"
      *> lexeme "{"
      *> ( foldl' (flip ($)) defaultParams
             <$> many pParam
         )
      <* lexeme "}"

symbolTerminal :: Symbol NT T -> Set T
symbolTerminal (Terminal t) = Set.singleton t
symbolTerminal (Nonterminal _) = mempty

ruleTerminals :: NTRule -> Set T
ruleTerminals = foldMap symbolTerminal . ruleProductions

cfgToGrammar ::
  CFG ->
  Either Text (ParsingGrammar NT T)
cfgToGrammar (CFG {ntRules = []}) = Left "CFG has no production rules."
cfgToGrammar (CFG {tRules, ntRules}) = do
  let productions = ruleProds <$> ntRules
      production_names =
        IntMap.fromList
          . catMaybes
          $ zipWith (liftA2 (,)) (pure <$> [0 :: Int ..])
          $ map ruleName ntRules
      nonterminals = List.nub $ map ruleNT ntRules
  start <-
    case List.uncons ntRules of
      Just (a, _) -> Right $ ruleNT a
      Nothing -> Left "CFG has no production rules."
  let terminals = List.nub $ map ruleT tRules ++ toList (foldMap ruleTerminals ntRules)
      grammar = Grammar {start, terminals, nonterminals, productions}
   in case grammarError grammar of
        Just err -> Left err
        Nothing ->
          Right $ parsingGrammar production_names grammar
  where
    ruleProds NTRule {ruleNT, ruleProductions} =
      Production ruleNT ruleProductions

implicitTRules :: CFG -> Either Text [TRule]
implicitTRules (CFG {tRules, ntRules}) = mapM implicitLitToRegEx implicit
  where
    declared = map ruleT tRules
    implicit = filter (`notElem` declared) $ toList $ foldMap ruleTerminals ntRules
    implicitLitToRegEx t@(TLit s) = Right $ TRule {ruleT = t, ruleRegex = regex}
      where
        regex = foldl1 Concat $ fmap (Literal . charToBytes) (Text.unpack s)
    implicitLitToRegEx (T s) = Left $ "Can not create literal from: " <> s

tRuleToTuple :: TRule -> (T, RegEx Bytes)
tRuleToTuple (TRule {ruleT = t, ruleRegex = regex}) = (t, regex)

cfgToDFALexerSpec :: CFG -> Either Text (DFALexerSpec Bytes Int T)
cfgToDFALexerSpec cfg@(CFG {tRules}) = do
  implicit_t_rules <- implicitTRules cfg
  let all_t_rules = implicit_t_rules ++ tRules
      t_rule_tuples = tRuleToTuple <$> all_t_rules
      x = find (producesEpsilon . snd) t_rule_tuples
  case x of
    Just (t, _) -> Left $ Text.pack [i|Error: #{t} may not produce empty strings.|]
    Nothing ->
      if null all_t_rules
        then Left "CFG has no lexical rules."
        else Right $ dfaLexerSpec 0 t_rule_tuples

type Parser = Parsec Void Text

space :: Parser ()
space = Lexer.space space1 (Lexer.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

pNT :: Parser NT
pNT = NT <$> p <?> "nonterminal (first letter must be uppercase)"
  where
    p = lexeme $ Text.cons <$> satisfy isAsciiUpper <*> takeWhileP Nothing ok
    ok c = c == '_' || isAsciiLower c || isAsciiUpper c || isDigit c

pName :: Parser Text
pName = lexeme $ char '[' *> (p <?> "production name (first letter must be uppercase)") <* char ']'
  where
    p = lexeme $ Text.cons <$> satisfy isAsciiUpper <*> takeWhileP Nothing ok
    ok c = c == '_' || isAsciiLower c || isAsciiUpper c || isDigit c

pStringLit :: Parser Text
pStringLit = lexeme $ char '"' *> takeWhile1P Nothing ok <* char '"'
  where
    ok c = isPrint c && c /= '"'

pT :: Parser T
pT = T <$> p <?> "terminal (first letter must be lowercase)"
  where
    p = lexeme $ Text.cons <$> satisfy isAsciiLower <*> takeWhileP Nothing ok
    ok c = c == '_' || isAsciiLower c || isAsciiUpper c || isDigit c

pTSym :: Parser T
pTSym =
  pT <|> (TLit <$> pStringLit) <?> "terminal"

pSymbol :: Parser (Symbol NT T)
pSymbol = Terminal <$> pTSym <|> Nonterminal <$> pNT

pTRule :: Parser TRule
pTRule =
  TRule
    <$> pT
    <* lexeme "="
    <*> (lexeme "/" *> pRegEx <* lexeme "/")
    <* lexeme "."
    <?> "terminal rule"

pNTRule :: Parser [NTRule]
pNTRule = map <$> pDef <*> (pRHS `sepBy` lexeme "|") <* lexeme "."
  where
    pDef :: Parser ([Symbol NT T] -> NTRule)
    pDef = NTRule <$> pNT <*> optional pName <* lexeme "->"
    pRHS = many pSymbol

pCFG :: Parser CFG
pCFG =
  CFG
    <$> try pParams
    <*> many (try pTRule)
    <*> (concat <$> many pNTRule)

cfgFromText :: FilePath -> Text -> Either Text CFG
cfgFromText fname s =
  either (Left . Text.pack . errorBundlePretty) Right $ parse (space *> pCFG <* eof) fname s

printRegEx :: RegEx Bytes -> Text
printRegEx Epsilon = ""
printRegEx (Literal c) = bytesToText c
printRegEx (Range cs) =
  printRegEx $ foldr1 Alter (fmap Literal cs)
printRegEx (Star r) = "(" <> printRegEx r <> ")*"
printRegEx (Concat r1 r2) =
  "(" <> printRegEx r1 <> " " <> printRegEx r2 <> ")"
printRegEx (Alter r1 r2) =
  "(" <> printRegEx r1 <> "|" <> printRegEx r2 <> ")"

printDfaSpec :: DFALexerSpec Bytes Int T -> Text
printDfaSpec spec =
  Text.intercalate "\n"
    . fmap (printTuple . snd)
    . List.sortOn fst
    $ toTuple <$> keys
  where
    ordmap = orderMap spec
    regmap = regexMap spec
    keys = Map.keys ordmap
    toTuple t = (ordmap Map.! t, (t, regmap Map.! t))
    printTuple (T t, r) = t <> " = " <> "/" <> printRegEx r <> "/" <> "."
    printTuple (TLit _, _) = error "Error: Cannot print a literal terminal rule."

printGrammar :: Grammar NT T -> Text
printGrammar grammar =
  Text.unlines $ printProduction <$> prods
  where
    prods = productions grammar
    printSymbol (Terminal (T t)) = t
    printSymbol (Terminal (TLit t)) = t
    printSymbol (Nonterminal (NT nt)) = nt

    printSymbols = Text.unwords . fmap printSymbol

    printProduction (Production (NT nt) syms) =
      nt <> " -> " <> printSymbols syms <> "."

parsePrinted :: DFALexerSpec Bytes Int T -> Bool
parsePrinted spec =
  case result of
    Left _ -> False
    Right b -> b
  where
    expected = mapSymbols unBytes spec
    result = do
      cfg <- cfgFromText "" $ printDfaSpec spec
      spec' <- cfgToDFALexerSpec cfg
      let spec'' = mapSymbols unBytes spec'
      pure $ dfaLexerSpecEquivalence (0 :: Integer) expected spec''

properties :: [(String, Property)]
properties =
  [("CFG properties", property parsePrinted)]

