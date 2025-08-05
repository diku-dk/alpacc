module Alpacc.Test.LexerParser (lexerParserTests) where

import Alpacc.CFG
import Alpacc.Encode
import Alpacc.Grammar
import Alpacc.LLP
import Alpacc.Lexer.DFA
import Alpacc.Lexer.FSA
import Alpacc.Util
import Data.Bifunctor
import Data.Binary
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text (Text)

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
      putNode (FlatTerminal p (t, (i, j))) = do
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
            pure $ FlatTerminal p (t, (i, j))
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

lexerParserTests :: CFG -> Int -> Int -> Int -> Either Text (ByteString, ByteString)
lexerParserTests cfg q k n = do
  spec <- dfaCharToWord8 <$> cfgToDFALexerSpec cfg
  grammar <- cfgToGrammar cfg
  table <- llpParserTableWithStarts q k $ getGrammar grammar
  let regex_map = regexMap spec
      ts = Map.keys regex_map
      encoder = encodeTerminals (T "ignore") $ parsingTerminals ts
      dfa = lexerDFA (0 :: Integer) spec
      ignore = T "ignore"
      alpha = alphabet $ fsa dfa
      comb = listProducts n $ Set.toList alpha
      inputs = toInputs comb
      maybe_ignore = if ignore `Map.member` regex_map then Just ignore else Nothing
      outputs = toOutputs encoder dfa maybe_ignore (getGrammar grammar) table comb
  pure
    ( ByteString.toStrict $ encode inputs,
      ByteString.toStrict $ encode outputs
    )
  where
    toOutputs encoder dfa ignore grammar table =
      Outputs . fmap (toOutput encoder dfa ignore grammar table)
    toInputs = Inputs . fmap (Input . ByteString.pack)
    toOutput encoder dfa ignore grammar table str = Output $ do
      ts <- fmap toTuple <$> tokenize dfa ignore str
      tree <- llpParseFlatTree grammar q k table (first Used <$> ts)
      pure $ fmap (fromIntegral . fromJust . (`terminalLookup` encoder)) <$> tree
      where
        toTuple (Lexeme t m) = (t, m)
