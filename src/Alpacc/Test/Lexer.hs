module Alpacc.Test.Lexer
  ( lexerTests,
  )
where

import Alpacc.CFG
import Alpacc.Encode
import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Alpacc.Lexer.FSA
import Alpacc.Types
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

data Outputs
  = Outputs
  { tokenType :: UInt,
    results :: [Output]
  }
  deriving (Show)

newtype Input = Input ByteString

newtype Inputs = Inputs [Input]

rep :: (Monad m) => Word64 -> m a -> m [a]
rep 0 _ = pure []
rep i m = (:) <$> m <*> rep (i - 1) m

putOutput :: UInt -> Output -> Put
putOutput _ (Output Nothing) = do
  put (False :: Bool)
putOutput token_type (Output (Just ts)) = do
  put (True :: Bool)
  put (fromIntegral $ length ts :: Word64)
  mapM_ putToken ts
  where
    putToken (Lexeme t (i, j)) = do
      case token_type of
        U8 -> put (fromIntegral t :: Word8)
        U16 -> put (fromIntegral t :: Word16)
        U32 -> put (fromIntegral t :: Word32)
        U64 -> put (fromIntegral t :: Word64)
      put i
      put j

getOutput :: UInt -> Get Output
getOutput token_type = do
  is_valid <- get :: Get Bool
  if is_valid
    then do
      num_tokens <- get :: Get Word64
      ts <- rep num_tokens getLexeme
      pure $ Output $ Just ts
    else pure $ Output Nothing
  where
    getLexeme = do
      t <-
        case token_type of
          U8 -> toInteger <$> (get :: Get Word8)
          U16 -> toInteger <$> (get :: Get Word16)
          U32 -> toInteger <$> (get :: Get Word32)
          U64 -> toInteger <$> (get :: Get Word64)
      i <- get :: Get Word64
      j <- get :: Get Word64
      pure $ Lexeme t (i, j)

instance Binary Input where
  put (Input str) = do
    put (fromIntegral $ ByteString.length str :: Word64)
    put str

  get = do
    i <- get :: Get Word64
    str <- ByteString.pack <$> rep i get
    pure $ Input str

instance Binary Inputs where
  put (Inputs inps) = do
    put (fromIntegral $ length inps :: Word64)
    mapM_ put inps

  get = do
    i <- get :: Get Word64
    inps <- rep i get
    pure $ Inputs inps

instance Binary Outputs where
  put (Outputs token_type results) = do
    put (fromIntegral $ numBits token_type :: Word64)
    put (fromIntegral $ length results :: Word64)
    mapM_ (putOutput token_type) results

  get = do
    num_bits <- get :: Get Word64
    token_type <-
      case num_bits of
        8 -> pure U8
        16 -> pure U16
        32 -> pure U32
        64 -> pure U64
        _any -> fail "Error: Only 8, 16, 32, and 64 are valid."
    i <- get :: Get Word64

    results <- rep i (getOutput token_type)
    pure $ Outputs token_type results

lexerTests :: CFG -> Int -> Either Text (ByteString, ByteString)
lexerTests cfg k = do
  spec <- dfaCharToWord8 <$> cfgToDFALexerSpec cfg
  let ts = Map.keys $ regexMap spec
      encoder = encodeTerminals (T "ignore") $ parsingTerminals ts
  token_type <- terminalIntType encoder
  dfa <-
    maybeToEither "Error: Could not encode tokens." $
      mapTokens (`terminalLookup` encoder) $
        lexerDFA (0 :: Integer) spec
  let ignore = terminalLookup (T "ignore") encoder
      alpha = alphabet $ fsa dfa
      comb = listProducts k $ Set.toList alpha
      outputs = toOutputs token_type dfa ignore comb
      inputs = toInputs comb
  pure
    ( ByteString.toStrict $ encode inputs,
      ByteString.toStrict $ encode outputs
    )
  where
    toOutputs token_type dfa ignore =
      Outputs token_type
        . fmap (Output . tokenize dfa ignore)
    toInputs = Inputs . fmap (Input . ByteString.pack)
