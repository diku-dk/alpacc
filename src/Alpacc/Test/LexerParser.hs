module Alpacc.Test.LexerParser
  ( lexerParserTests,
    parse,
    lexerParserTestsCompare,
  )
where

import Alpacc.CFG
import Alpacc.Encode
import Alpacc.Grammar
import Alpacc.LL (generateRandomDerivation)
import Alpacc.LLP
import Alpacc.Lexer.DFA
import Alpacc.Lexer.FSA
import Alpacc.Lexer.RegularExpression
import Alpacc.Test.Lexer (TestMode (..), randomSeed)
import Alpacc.Util
import Codec.Binary.UTF8.String (encodeChar)
import Control.Monad
import Data.Array (Array, bounds, listArray, (!))
import Data.Bifunctor
import Data.Binary
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal
import Data.Either.Extra
import Data.Foldable
import Data.List (zip4)
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import Data.Maybe
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq hiding (Seq (..), (<|), (><), (|>))
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
      encoder = fromSymbolToTerminalEncoder $ encodeSymbols ignore grammar
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

-- | Compute the shortest byte sequence that lexes to each token in
-- the DFA.  Uses BFS from the initial state, following only original
-- (non-producing) transitions so that the accumulated bytes never
-- cross a token boundary.
computeTokenBytesMap ::
  (Ord s) =>
  Set.Set Word8 ->
  DFALexer Word8 s T ->
  Map T [Word8]
computeTokenBytesMap _alpha dfa_lexer =
  go (Seq.singleton (initial_state, [])) (Set.singleton initial_state) Map.empty
  where
    dfa = fsa dfa_lexer
    initial_state = initial dfa
    trans = transitions' dfa
    accept_states = accepting dfa
    token_map = tokenMap dfa_lexer
    produces = producesToken dfa_lexer

    trans_by_state =
      Map.fromListWith
        Map.union
        [ (s, Map.singleton sym s')
        | ((s, sym), s') <- Map.toList trans
        ]

    go Empty _ result = result
    go ((state, path) :<| queue) visited result =
      let result' = case Map.lookup state token_map of
            Just tok
              | Set.member state accept_states,
                not (Map.member tok result) ->
                  Map.insert tok (reverse path) result
            _ -> result
          neighbors =
            Seq.fromList
              [ (next_state, sym : path)
              | (sym, next_state) <-
                  Map.toList $
                    Map.findWithDefault Map.empty state trans_by_state,
                not (next_state `Set.member` visited),
                not ((state, sym) `Set.member` produces)
              ]
          visited' = Set.union visited (Set.fromList (fst <$> toList neighbors))
       in go (queue <> neighbors) visited' result'

-- | Compute the per-token budget for extra self-loop characters.
-- Distributes the total target length evenly across the token list.
calculateMaxExtra :: Int -> [a] -> Int
calculateMaxExtra len tokens = max 1 (len `div` max 1 (length tokens))

-- | Generate a varied-length byte sequence that the DFA lexer will
-- lex as the given token. The minimum (shortest) path to the
-- accepting state is taken as a baseline; at each intermediate state
-- that has self-loop transitions (i.e. transitions on some symbol
-- that lead back to the same state) extra characters are randomly
-- inserted. This produces non-trivial token values such as non-empty
-- strings, multi-digit numbers, etc.
--
-- The @maxExtra@ parameter bounds the total number of inserted
-- characters so the function always terminates. The updated generator
-- is also returned so callers can thread randomness across multiple
-- tokens.
generateVariedTokenBytes ::
  (Ord s, RandomGen g) =>
  Map T [Word8] ->
  Map s (Array Int Word8) ->
  g ->
  Int ->
  T ->
  Set.Set Word8 ->
  DFALexer Word8 s T ->
  ([Word8], g)
generateVariedTokenBytes token_bytes_map self_loop_map gen maxExtra tok _alpha dfa_lexer =
  go gen initial_state 0 [] min_bytes
  where
    min_bytes = Map.findWithDefault [] tok token_bytes_map
    dfa_fsa = fsa dfa_lexer
    trans = transitions' dfa_fsa

    initial_state = initial dfa_fsa

    emptyArr = listArray (0, -1) []

    selfLoops state = Map.findWithDefault emptyArr state self_loop_map

    go g _state _extraSoFar acc [] = (reverse acc, g)
    go g state extraSoFar acc (sym : rest) =
      let loops = selfLoops state
          (g', new_count, extra) = addLoops g extraSoFar loops
          next_state = Map.findWithDefault state (state, sym) trans
          new_acc = sym : foldl' (flip (:)) acc extra
       in go g' next_state new_count new_acc rest

    addLoops g count loops
      | count >= maxExtra = (g, count, [])
      | n == 0 = (g, count, [])
      | otherwise =
          let (r, g') = randomR (0 :: Int, 2) g
           in if r == 0
                then
                  let (idx, g'') = randomR (0, n - 1) g'
                      sym = loops ! idx
                      (g''', new_count, more) = addLoops g'' (count + 1) loops
                   in (g''', new_count, sym : more)
                else (g', count, [])
      where
        (lo, hi) = bounds loops
        n = hi - lo + 1

-- | DFS based approach to generating input. This generates a valid
-- token sequence efficiently and then generates byte sequences that
-- will lex to those tokens.
generateSingleLongLexerParserInput ::
  (Ord s, Ord nt, Show nt) =>
  Int ->
  Set.Set Word8 ->
  DFALexer Word8 s T ->
  Grammar (AugmentedNonterminal (Symbol nt T)) (AugmentedTerminal (Unused T)) ->
  [Word8]
generateSingleLongLexerParserInput len alpha dfa_lexer grammar =
  let gen = mkStdGen randomSeed
      token_bytes_map = computeTokenBytesMap alpha dfa_lexer
      trans = transitions' $ fsa dfa_lexer
      produces = producesToken dfa_lexer
      self_loop_map =
        Map.map (\xs -> listArray (0, length xs - 1) xs) $
          Map.fromListWith
            (++)
            [ (s, [sym])
            | ((s, sym), s') <- Map.toList trans,
              s == s',
              not ((s, sym) `Set.member` produces)
            ]

      unaug (AugmentedTerminal (Used t)) = Just t
      unaug _ = Nothing

      -- Find terminals that are in both the lexer and the grammar
      (gen2, derivedTerminals) = generateRandomDerivation gen len grammar
      tokens = mapMaybe unaug derivedTerminals

      -- Generate varied bytes for each token, threading the generator
      -- so each occurrence gets different content (e.g. non-empty strings).
      maxExtra = calculateMaxExtra len tokens
      (_, bytesRev) =
        foldl
          ( \(g, acc) tok ->
              let (bs, g') = generateVariedTokenBytes token_bytes_map self_loop_map g maxExtra tok alpha dfa_lexer
               in (g', bs : acc)
          )
          (gen2, [])
          tokens
   in concat (reverse bytesRev)

lexerParserTests :: TestMode -> CFG -> Int -> Either Text (ByteString, ByteString)
lexerParserTests mode cfg n = do
  let q = paramsLookback $ cfgParams cfg
      k = paramsLookahead $ cfgParams cfg
  spec <- cfgToDFALexerSpec cfg
  grammar <- cfgToGrammar cfg
  table <- llpParserTableWithStarts q k $ getGrammar grammar
  let regex_map = regexMap spec
      ignore = T "ignore"
      encoder = fromSymbolToTerminalEncoder $ encodeSymbols ignore grammar
      dfa = lexerDFA (0 :: Integer) $ mapSymbols unBytes spec
      alpha = alphabet $ fsa dfa
      maybe_ignore = if ignore `Map.member` regex_map then Just ignore else Nothing
      (inputs, outputs) = case mode of
        Exhaustive ->
          let comb = listProducts n $ Set.toList alpha
           in (toInputs comb, toOutputs q k encoder dfa maybe_ignore (getGrammar grammar) table comb)
        SingleLong ->
          let singleInput = generateSingleLongLexerParserInput n alpha dfa (getGrammar grammar)
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
  grammar <- cfgToGrammar cfg
  let ignore = T "ignore"
      encoder = fromSymbolToTerminalEncoder $ encodeSymbols ignore grammar
      int_to_token =
        Map.fromList
          [ (fromIntegral i, t)
          | (i, Used t) <- zip [0 :: Integer ..] (toTerminals encoder)
          ]
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
