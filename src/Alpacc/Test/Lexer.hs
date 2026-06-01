module Alpacc.Test.Lexer
  ( lexerTests,
    lexerTestsCompare,
    TestMode (..),
    randomSeed,
    generateParseableInputFromDFA,
  )
where

import Alpacc.CFG
import Alpacc.Encode
import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Alpacc.Lexer.FSA
import Alpacc.Lexer.RegularExpression
import Alpacc.Util
import Control.Monad
import Data.Bifunctor
import Data.Binary
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal
import Data.Either.Extra
import Data.List (zip4)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import System.Random

-- | Seed used for reproducible random generation in test cases
randomSeed :: Int
randomSeed = 42

-- | Test generation mode.
-- 'Exhaustive' generates all possible input combinations up to the specified length.
-- This is useful for comprehensive testing but becomes impractical for lengths > 7.
-- 'SingleLong' generates a single random input of exactly the specified length.
-- This is useful for performance testing and stress testing with long inputs.
data TestMode
  = Exhaustive
  | SingleLong
  deriving (Show, Eq)

newtype Output
  = Output
  { result :: Maybe [Lexeme Word64]
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
        t <- get :: Get Word64
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

-- | Generate a parseable input by simulating the DFA.
-- Starting from the initial state, randomly choose valid transitions
-- until we reach the desired length and are in an accepting state.
generateParseableInputFromDFA :: (Ord s, Ord t) => Int -> Set.Set t -> DFA t s -> [t]
generateParseableInputFromDFA len alpha dfa =
  let gen = mkStdGen randomSeed
      initial_state = initial dfa
      trans = transitions' dfa
      accept = accepting dfa
      alphaList = Set.toList alpha
      
      simulateDFA _ 0 state acc
        | state `Set.member` accept = reverse acc
        | otherwise = reverse acc -- Best effort, return what we have
      simulateDFA g n state acc =
        -- Get all valid next symbols from current state
        let validSymbols = [sym | sym <- alphaList, Map.member (state, sym) trans]
         in if null validSymbols
              then reverse acc -- Stuck, return what we have
              else
                let (idx, g') = randomR (0, length validSymbols - 1) g
                    nextSymbol = validSymbols !! idx
                    nextState = trans Map.! (state, nextSymbol)
                 in simulateDFA g' (n - 1) nextState (nextSymbol : acc)
   in simulateDFA gen len initial_state []

-- | Generate a single random input of given length from the alphabet.
-- Returns an empty list if the alphabet is empty or length is 0.
generateSingleLongInput :: Int -> Set.Set Word8 -> [Word8]
generateSingleLongInput _ alpha | Set.null alpha = []
generateSingleLongInput len alpha =
  let gen = mkStdGen randomSeed
      alphaList = Set.toList alpha
      numChoices = length alphaList
      randomIndices = take len $ randomRs (0, numChoices - 1) gen
   in map (alphaList !!) randomIndices

lexerTests :: TestMode -> Bool -> CFG -> Int -> Either Text (ByteString, ByteString)
lexerTests mode parseable cfg k = do
  spec <- cfgToDFALexerSpec cfg
  let ts = Map.keys $ regexMap spec
      encoder = encodeTerminals (T "ignore") $ parsingTerminals ts
  dfa <-
    maybeToEither "Error: Could not encode tokens." $
      mapTokens (fmap fromIntegral . (`terminalLookup` encoder)) $
        lexerDFA (0 :: Integer) $
          mapSymbols unBytes spec
  let ignore = fromIntegral <$> terminalLookup (T "ignore") encoder
      alpha = alphabet $ fsa dfa
      (inputs, outputs) = case mode of
        Exhaustive ->
          let comb = listProducts k $ Set.toList alpha
           in (toInputs comb, toOutputs dfa ignore comb)
        SingleLong ->
          let singleInput = if parseable
                              then generateParseableInputFromDFA k alpha (fsa dfa)
                              else generateSingleLongInput k alpha
           in (toInputs [singleInput], toOutputs dfa ignore [singleInput])
  pure
    ( ByteString.toStrict $ encode inputs,
      ByteString.toStrict $ encode outputs
    )
  where
    toOutputs dfa ignore = Outputs . fmap (Output . tokenize dfa ignore)
    toInputs = Inputs . fmap (Input . ByteString.pack)

lexerTestsCompare :: CFG -> ByteString -> ByteString -> ByteString -> Either Text ()
lexerTestsCompare cfg input expected result = do
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

    showLexeme int_to_token (Lexeme t sp) = do
      t' <- maybeToEither err $ Map.lookup t int_to_token
      pure $ Text.pack $ show (t', sp)
      where
        err = "Error: Could not find the token with encoding '" <> Text.pack (show t) <> "'"

    showOutput _ Nothing = Right "Unable to parse."
    showOutput int_to_token (Just lexemes) = do
      ts <- mapM (showLexeme int_to_token) lexemes
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
