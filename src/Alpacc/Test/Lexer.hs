module Alpacc.Test.Lexer
  ( lexerTests,
  )
where

import Alpacc.CFG
import Alpacc.Encode
import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Alpacc.Lexer.FSA
import Alpacc.Util
import Data.Binary
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal
import Data.Either.Extra
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)

newtype Output
  = Output
  { result :: Maybe [Lexeme Integer]
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
    put (fromIntegral $ ts :: Word64)
    mapM_ putToken ts
    where
      putToken (Lexeme t (i, j)) = do
        put (fromIntegral t :: Word64)
        put i
        put j

  get = do
    is_valid <- get :: Get Bool
    if is_valid
      then do
        num_tokens <- get :: Get Word64
        ts <- mapM (const getLexeme) [1 .. num_tokens]
        pure $ Output $ Just ts
      else pure $ Output Nothing
    where
      getLexeme = do
        t <- toInteger <$> (get :: Get Word64)
        i <- get :: Get Word64
        j <- get :: Get Word64
        pure $ Lexeme t (i, j)

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

lexerTests :: CFG -> Int -> Either Text (ByteString, ByteString)
lexerTests cfg k = do
  spec <- dfaCharToWord8 <$> cfgToDFALexerSpec cfg
  let ts = Map.keys $ regexMap spec
      encoder = encodeTerminals (T "ignore") $ parsingTerminals ts
  dfa <-
    maybeToEither "Error: Could not encode tokens." $
      mapTokens (`terminalLookup` encoder) $
        lexerDFA (0 :: Integer) spec
  let ignore = terminalLookup (T "ignore") encoder
      alpha = alphabet $ fsa dfa
      comb = listProducts k $ Set.toList alpha
      inputs = toInputs comb
      outputs = toOutputs dfa ignore comb
  pure
    ( ByteString.toStrict $ encode inputs,
      ByteString.toStrict $ encode outputs
    )
  where
    toOutputs dfa ignore = Outputs . fmap (Output . tokenize dfa ignore)
    toInputs = Inputs . fmap (Input . ByteString.pack)
