module Alpacc.Test.Parser
  ( parserTests,
    parserTestsCompare,
    generateParseableTokenSequence,
    generateParseableTokenSequenceFast,
  )
where

import Alpacc.CFG
import Alpacc.Encode
import Alpacc.Grammar
import Alpacc.LLP
import Alpacc.LL (derivableNLengths, generateRandomDerivation)
import Alpacc.Test.Lexer (TestMode (..), randomSeed)
import Alpacc.Util
import Control.Monad
import Data.Bifunctor
import Data.Binary
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal
import Data.List (zip4)
import Data.Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import System.Random

newtype Output
  = Output
  { result :: Maybe [Word64]
  }
  deriving (Show)

newtype Outputs
  = Outputs
  { results :: [Output]
  }
  deriving (Show)

newtype Input = Input [Word64] deriving (Show)

newtype Inputs = Inputs [Input] deriving (Show)

instance Binary Output where
  put (Output Nothing) =
    put (False :: Bool)
  put (Output (Just prods)) = do
    put (True :: Bool)
    put (fromIntegral $ length prods :: Word64)
    mapM_ put prods

  get = do
    is_valid <- get :: Get Bool
    if is_valid
      then do
        num_prods <- get :: Get Word64
        prods <- mapM (const (get :: Get Word64)) [1 .. num_prods]
        pure $ Output $ Just prods
      else pure $ Output Nothing

instance Binary Input where
  put (Input tokens) = do
    put (fromIntegral $ length tokens :: Word64)
    mapM_ put tokens

  get = do
    i <- get :: Get Word64
    tokens <- mapM (const get) [1 .. i]
    pure $ Input tokens

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

-- | Generate a parseable token sequence using derivations from the grammar.
-- Start with the start symbol and randomly choose productions until we
-- derive a string of terminals of the desired length.
generateParseableTokenSequence :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar (AugmentedNonterminal nt) (AugmentedTerminal t) -> [t] -> [t]
generateParseableTokenSequence _ _ [] = []
generateParseableTokenSequence len grammar terminals =
  let gen = mkStdGen randomSeed
      -- Get all derivations of length up to len from the start symbol
      -- We use len to ensure we get all derivations up to and including length len
      derivable = derivableNLengths len grammar
   in if Set.null derivable
        then generateSingleLongTokenSequence len terminals
        else
          let derivableList = Set.toList derivable
              validDerivations = filter ((== len) . length) derivableList
           in if null validDerivations
                then generateSingleLongTokenSequence len terminals
                else
                  let (idx, _) = randomR (0, length validDerivations - 1) gen
                   in mapMaybe unaug $ validDerivations !! idx
  where
    unaug (AugmentedTerminal t) = Just t
    unaug _ = Nothing

-- | Fast version of generateParseableTokenSequence using the new DFS-based approach.
-- This avoids enumerating all derivations and instead generates one randomly using
-- a smart strategy that switches between expansion and convergence.
generateParseableTokenSequenceFast :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar (AugmentedNonterminal nt) (AugmentedTerminal t) -> [t] -> [t]
generateParseableTokenSequenceFast _ _ [] = []
generateParseableTokenSequenceFast len grammar terminals =
  let gen = mkStdGen randomSeed
      (_, derivedTerminals) = generateRandomDerivation gen len grammar
      -- Filter out augmented terminals
      result = mapMaybe unaug $ map AugmentedTerminal derivedTerminals
   in if null result
        then generateSingleLongTokenSequence len terminals
        else take len result ++ generateSingleLongTokenSequence (len - length result) terminals
  where
    unaug (AugmentedTerminal t) = Just t
    unaug _ = Nothing

-- | Generate a single random token sequence of given length.
-- Returns an empty list if the terminals list is empty or length is 0.
generateSingleLongTokenSequence :: Int -> [a] -> [a]
generateSingleLongTokenSequence _ [] = []
generateSingleLongTokenSequence len terminals =
  let gen = mkStdGen randomSeed
      numTerms = length terminals
      randomIndices = take len $ randomRs (0, numTerms - 1) gen
   in map (terminals !!) randomIndices

parserTests :: TestMode -> Bool -> CFG -> Int -> Either Text (ByteString, ByteString)
parserTests mode parseable cfg n = do
  let q = paramsLookback $ cfgParams cfg
      k = paramsLookahead $ cfgParams cfg
  grammar <- cfgToGrammar cfg
  table <- llpParserTableWithStarts q k $ getGrammar grammar
  let s_encoder = encodeSymbols (T "ignore") grammar
      p x =
        x /= AugmentedTerminal Unused
          && x /= LeftTurnstile
          && x /= RightTurnstile
      encode' x = fromJust $ Terminal x `symbolLookup` s_encoder
      validTerminals =
        mapMaybe unaug $
          filter p $
            terminals $
              getGrammar grammar
      (inputs, outputs) = case mode of
        Exhaustive ->
          let comb = listProducts n validTerminals
           in ( Inputs $ Input . fmap (fromIntegral . encode' . AugmentedTerminal) <$> comb,
                Outputs $ Output . fmap (fmap fromIntegral) . parse <$> comb
              )
        SingleLong ->
          let singleSeq = if parseable
                            then if n > 5
                                   then generateParseableTokenSequenceFast n (getGrammar grammar) validTerminals
                                   else generateParseableTokenSequence n (getGrammar grammar) validTerminals
                            else generateSingleLongTokenSequence n validTerminals
              comb = [singleSeq]
           in ( Inputs $ Input . fmap (fromIntegral . encode' . AugmentedTerminal) <$> comb,
                Outputs $ Output . fmap (fmap fromIntegral) . parse <$> comb
              )
      parse = llpParse q k table
  pure
    ( ByteString.toStrict $ encode inputs,
      ByteString.toStrict $ encode outputs
    )
  where
    unaug (AugmentedTerminal t) = Just t
    unaug _ = Nothing

parserTestsCompare :: ByteString -> ByteString -> ByteString -> Either Text ()
parserTestsCompare input expected result = do
  Inputs inp <- dec "Error: Could not parse input file." input
  Outputs ex <- dec "Error: Could not parse expected output file." expected
  Outputs res <- dec "Error: Could not parse result output file." result
  failwith (length inp == length ex) "Error: Input and expected output file do not have the same number of tests."
  failwith (length inp == length res) "Error: Input and result output file do not have the same number of tests."

  mapM_ compareTest $ zip4 [0 :: Integer ..] inp ex res
  where
    dec str =
      bimap (const str) (\(_, _, a) -> a)
        . decodeOrFail
        . ByteString.fromStrict

    failwith b s = unless b (Left s)

    showOutput Nothing = "Unable to parse."
    showOutput (Just ps) = Text.unwords $ Text.pack . show <$> ps

    compareTest (idx, Input i, Output e, Output r) = do
      failwith (e == r) $
        Text.unlines
          [ "Failed on Input: " <> Text.pack (show i),
            "Input Index: " <> Text.pack (show idx),
            "Expected: " <> showOutput e,
            "Got: " <> showOutput r
          ]
