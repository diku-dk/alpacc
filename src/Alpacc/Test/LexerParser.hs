module Alpacc.Test.LexerParser
  ( lexerParserTests,
    parse,
    lexerParserTestsCompare,
    generateParseableLexerParserInput,
  )
where

import Alpacc.CFG
import Alpacc.Encode
import Alpacc.Grammar
import Alpacc.LLP
import Alpacc.LL (derivableNLengths)
import Alpacc.Lexer.DFA
import Alpacc.Lexer.FSA
import Alpacc.Lexer.RegularExpression
import Alpacc.Test.Lexer (TestMode (..), randomSeed)
import Alpacc.Util
import Codec.Binary.UTF8.String (encodeChar)
import Control.Monad
import Data.Bifunctor
import Data.Binary
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal
import Data.Either.Extra
import Data.List (zip4)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import System.Random

newtype Output
  = Output
  { result :: Maybe [FlatNode Word64 (Word64, Word64) Word64]
  }
  deriving (Show)

newtype Outputs
  = Outputs
  { results :: [Output]
  }
  deriving (Show)

newtype Input = Input ByteString deriving (Show)

newtype Inputs = Inputs [Input] deriving (Show)

instance Binary Output where
  put (Output Nothing) =
    put (False :: Bool)
  put (Output (Just ts)) = do
    put (True :: Bool)
    put (fromIntegral $ length ts :: Word64)
    mapM_ putNode ts
    where
      putNode (FlatProduction p t) = do
        put (0 :: Word8)
        put p
        put t
        put (0 :: Word64)
        put (0 :: Word64)
      putNode (FlatTerminal p (i, j) t) = do
        put (1 :: Word8)
        put p
        put t
        put i
        put j

  get = do
    is_valid <- get :: Get Bool
    if is_valid
      then do
        num_tokens <- get :: Get Word64
        ns <- mapM (const getNode) [1 .. num_tokens]
        pure $ Output $ Just ns
      else pure $ Output Nothing
    where
      getNode = do
        node_type <- get :: Get Word8
        case node_type of
          0 -> do
            p <- get :: Get Word64
            t <- get :: Get Word64
            0 <- get :: Get Word64
            0 <- get :: Get Word64
            pure $ FlatProduction p t
          1 -> do
            p <- get :: Get Word64
            t <- get :: Get Word64
            i <- get :: Get Word64
            j <- get :: Get Word64
            pure $ FlatTerminal p (i, j) t
          _ -> fail "Error: Could not parse input due to invalid CST node type."

instance Binary Input where
  put (Input str) = do
    put (fromIntegral $ ByteString.length str :: Word64)
    mapM_ put $ ByteString.unpack str

  get = do
    i <- get :: Get Word64
    str <- ByteString.pack <$> mapM (const get) [1 .. i]
    pure $ Input str

instance Binary Inputs where
  put (Inputs inps) = do
    put (fromIntegral $ length inps :: Word64)
    mapM_ put inps

  get = do
    i <- get :: Get Word64
    inps <- mapM (const get) [1 .. i]
    pure $ Inputs inps

instance Binary Outputs where
  put (Outputs results) = do
    put (fromIntegral $ length results :: Word64)
    mapM_ put results

  get = do
    i <- get :: Get Word64
    results <- mapM (const get) [1 .. i]
    pure $ Outputs results

parse :: CFG -> Int -> Int -> Text -> Either Text (Maybe [FlatNode Word64 (Word64, Word64) Word64])
parse cfg q k str = do
  spec <- cfgToDFALexerSpec cfg
  grammar <- cfgToGrammar cfg
  table <- llpParserTableWithStarts q k $ getGrammar grammar
  let regex_map = regexMap spec
      ignore = T "ignore"
      ters = Map.keys regex_map
      encoder = encodeTerminals (T "ignore") $ parsingTerminals ters
      maybe_ignore = if ignore `Map.member` regex_map then Just ignore else Nothing
      dfa = lexerDFA (0 :: Integer) $ mapSymbols unBytes spec
      bytes = concatMap encodeChar $ Text.unpack str
      ts = fmap toTuple <$> tokenize dfa maybe_ignore bytes
  case ts of
    Just ts' ->
      let tree = llpParseFlatTree (getGrammar grammar) q k table (first Used <$> ts')
       in pure $ fmap (fmap (fromIntegral . fromJust . (`terminalLookup` encoder))) <$> tree
    Nothing -> pure Nothing
  where
    toTuple (Lexeme t m) = (t, m)

-- | Generate a parseable lexer+parser input by simulating both together.
-- This is trickier: we need to generate a string that both lexes correctly
-- and parses correctly. Strategy: first generate a valid token sequence using
-- grammar derivations, then generate byte sequences that lex to those tokens.
generateParseableLexerParserInput ::
  (Ord s, Ord nt, Show nt) =>
  Int ->
  Set.Set Word8 ->
  DFALexer Word8 s T ->
  Maybe T ->
  Grammar (AugmentedNonterminal (Symbol nt T)) (AugmentedTerminal (Unused T)) ->
  [Word8]
generateParseableLexerParserInput len alpha dfa_lexer maybe_ignore grammar =
  let gen = mkStdGen randomSeed
      token_map_dfa = tokenMap dfa_lexer
      
      -- Get terminals that can be produced by the lexer
      all_lexer_terminals = Map.elems token_map_dfa
      lexer_terminals = case maybe_ignore of
        Just ignore -> filter (/= ignore) all_lexer_terminals
        Nothing -> all_lexer_terminals
      
      -- Get valid terminals from the grammar
      validGrammarTerminals =
        mapMaybe unaug $
          filter p $
            terminals grammar
      
      p x = x /= AugmentedTerminal Unused && x /= LeftTurnstile && x /= RightTurnstile
      unaug (AugmentedTerminal (Used t)) = Just t
      unaug _ = Nothing
      
      -- Find terminals that are in both the lexer and the grammar
      commonTerminals = filter (`elem` lexer_terminals) validGrammarTerminals
      commonTerminalsSet :: Set.Set (AugmentedTerminal (Unused T))
      commonTerminalsSet = Set.fromList $ map (AugmentedTerminal . Used) commonTerminals
      
   in if null commonTerminals
        then generateSingleLongLexerParserInput len alpha
        else
          -- Generate a sequence of tokens using grammar derivations
          -- We use len to get derivations of exactly that length
          let derivable = derivableNLengths len grammar
              derivableList = Set.toList derivable
              -- Filter to get derivations that use our common terminals
              validDerivations = filter (\d -> all (`Set.member` commonTerminalsSet) (take len d)) derivableList
           in if null validDerivations
                then generateSingleLongLexerParserInput len alpha
                else
                  -- Pick a random derivation
                  let (idx, gen') = randomR (0, length validDerivations - 1) gen
                      chosen = take len $ validDerivations !! idx
                      tokens = mapMaybe unaug chosen
                   in generateBytesForTokens gen' tokens
  where
    -- Generate byte sequences that will lex to the given token sequence
    -- Using an accumulator to avoid O(n²) list concatenation
    generateBytesForTokens _ [] = []
    generateBytesForTokens g (tok:toks) =
      let (bytes, g') = generateBytesForToken g tok
       in bytes ++ generateBytesForTokens g' toks
    
    -- Generate bytes that lex to a specific token
    -- Returns the generated bytes and the updated random generator
    generateBytesForToken :: RandomGen g => g -> T -> ([Word8], g)
    generateBytesForToken g target_token =
      let dfa_local = fsa dfa_lexer
          initial_state_local = initial dfa_local
          trans_local = transitions' dfa_local
          accept_local = accepting dfa_local
          token_map_local = tokenMap dfa_lexer
          produces_token_set_local = producesToken dfa_lexer
          alphaList_local = Set.toList alpha
          
          simulateForToken gen state acc target
            | state `Set.member` accept_local,
              Just tok <- Map.lookup state token_map_local,
              tok == target = (reverse acc, gen)
            | otherwise =
                let validSymbols = [sym | sym <- alphaList_local, Map.member (state, sym) trans_local]
                 in if null validSymbols
                      then (reverse acc, gen)
                      else
                        let (idx, gen') = randomR (0, length validSymbols - 1) gen
                            nextSymbol = validSymbols !! idx
                            nextState = trans_local Map.! (state, nextSymbol)
                            -- Check if this transition produces our target token
                            shouldStop = (nextState, nextSymbol) `Set.member` produces_token_set_local
                                      && Map.lookup nextState token_map_local == Just target
                         in if shouldStop
                              then (reverse (nextSymbol : acc), gen')
                              else simulateForToken gen' nextState (nextSymbol : acc) target
       in simulateForToken g initial_state_local [] target_token

-- | Generate a single random input of given length from the alphabet.
-- Returns an empty list if the alphabet is empty or length is 0.
generateSingleLongLexerParserInput :: Int -> Set.Set Word8 -> [Word8]
generateSingleLongLexerParserInput _ alpha | Set.null alpha = []
generateSingleLongLexerParserInput len alpha =
  let gen = mkStdGen randomSeed
      alphaList = Set.toList alpha
      numChoices = length alphaList
      randomIndices = take len $ randomRs (0, numChoices - 1) gen
   in map (alphaList !!) randomIndices

lexerParserTests :: TestMode -> Bool -> CFG -> Int -> Either Text (ByteString, ByteString)
lexerParserTests mode parseable cfg n = do
  let q = paramsLookback $ cfgParams cfg
      k = paramsLookahead $ cfgParams cfg
  spec <- cfgToDFALexerSpec cfg
  grammar <- cfgToGrammar cfg
  table <- llpParserTableWithStarts q k $ getGrammar grammar
  let regex_map = regexMap spec
      ts = Map.keys regex_map
      encoder = encodeTerminals (T "ignore") $ parsingTerminals ts
      dfa = lexerDFA (0 :: Integer) $ mapSymbols unBytes spec
      ignore = T "ignore"
      alpha = alphabet $ fsa dfa
      maybe_ignore = if ignore `Map.member` regex_map then Just ignore else Nothing
      (inputs, outputs) = case mode of
        Exhaustive ->
          let comb = listProducts n $ Set.toList alpha
           in (toInputs comb, toOutputs q k encoder dfa maybe_ignore (getGrammar grammar) table comb)
        SingleLong ->
          let singleInput = if parseable
                              then generateParseableLexerParserInput n alpha dfa maybe_ignore (getGrammar grammar)
                              else generateSingleLongLexerParserInput n alpha
              comb = [singleInput]
           in (toInputs comb, toOutputs q k encoder dfa maybe_ignore (getGrammar grammar) table comb)
  pure
    ( ByteString.toStrict $ encode inputs,
      ByteString.toStrict $ encode outputs
    )
  where
    toOutputs q k encoder dfa ignore grammar table =
      Outputs . fmap (toOutput q k encoder dfa ignore grammar table)
    toInputs = Inputs . fmap (Input . ByteString.pack)
    toOutput q k encoder dfa ignore grammar table str = Output $ do
      ts <- fmap toTuple <$> tokenize dfa ignore str
      tree <- llpParseFlatTree grammar q k table (first Used <$> ts)
      pure $ fmap (fromIntegral . fromJust . (`terminalLookup` encoder)) <$> tree
      where
        toTuple (Lexeme t m) = (t, m)

lexerParserTestsCompare :: CFG -> ByteString -> ByteString -> ByteString -> Either Text ()
lexerParserTestsCompare cfg input expected result = do
  spec <- cfgToDFALexerSpec cfg

  let ts = Map.keys $ regexMap spec
      encoder = encodeTerminals (T "ignore") $ parsingTerminals ts
  encodings <-
    maybeToEither "Error: Could not encode tokens." $
      mapM (fmap fromIntegral . (`terminalLookup` encoder)) ts
  let int_to_token = Map.fromList $ zip encodings ts
  Inputs inp <- dec "Error: Could not parse input file." input
  Outputs ex <- dec "Error: Could not parse expected output file." expected
  Outputs res <- dec "Error: Could not parse result output file." result
  failwith (length inp == length ex) "Error: Input and expected output file do not have the same number of tests."
  failwith (length inp == length res) "Error: Input and result output file do not have the same number of tests."

  mapM_ (compareTest int_to_token) $ zip4 [0 :: Integer ..] inp ex res
  where
    dec str =
      bimap (const str) (\(_, _, a) -> a)
        . decodeOrFail
        . ByteString.fromStrict

    failwith b s = unless b (Left s)

    showNode _ p@(FlatProduction _ _) = pure $ Text.pack $ show p
    showNode int_to_token (FlatTerminal p sp t) = do
      t' <- maybeToEither err $ Map.lookup t int_to_token
      pure $ Text.pack $ show $ FlatTerminal p sp t'
      where
        err = "Error: Could not find the token with encoding '" <> Text.pack (show t) <> "'"

    showOutput _ Nothing = Right "Unable to parse."
    showOutput int_to_token (Just nodes) = do
      ts <- mapM (showNode int_to_token) nodes
      pure $ Text.unwords ts

    compareTest int_to_token (idx, Input i, Output e, Output r) = do
      failwith (e == r) $
        case (showOutput int_to_token e, showOutput int_to_token r) of
          (Right e', Right r') ->
            Text.unlines
              [ "Failed on Input: " <> Text.pack (show i),
                "Input Index: " <> Text.pack (show idx),
                "Expected: " <> e',
                "Got: " <> r'
              ]
          (Left s, _) -> s
          (_, Left s) -> s
