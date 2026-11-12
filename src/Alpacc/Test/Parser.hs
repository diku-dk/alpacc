module Alpacc.Test.Parser
  ( parserTests,
    parserTestsSingleLong,
    parserTestsCompare,
  )
where

import Alpacc.CFG
import Alpacc.Encode
import Alpacc.Grammar
import Alpacc.Parser.LL (generateRandomDerivation, generateRandomDerivationLazy)
import Alpacc.Parser.LLP
import Alpacc.Test.Lexer (TestMode (..), decodeWith, getUInt, putUInt, randomSeed, uintBytes)
import Alpacc.Types
import Alpacc.Util
import Control.Monad
import Data.Binary.Get (Get, getWord64le, getWord8)
import Data.Binary.Put (Put, putWord64le, putWord8, runPut)
import Data.ByteString.Internal
import Data.ByteString.Lazy qualified as LBS
import Data.List (zip4)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import System.IO (Handle, SeekMode (..), hSeek)
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

putOutput :: UInt -> Output -> Put
putOutput _ (Output Nothing) = putWord8 0
putOutput pw (Output (Just prods)) = do
  putWord8 1
  putWord64le (fromIntegral $ length prods)
  mapM_ (putUInt pw) prods

getOutput :: UInt -> Get Output
getOutput pw = do
  is_valid <- getWord8
  if is_valid == 1
    then do
      num_prods <- getWord64le
      Output . Just <$> replicateM (fromIntegral num_prods) (getUInt pw)
    else pure $ Output Nothing

putInput :: UInt -> Input -> Put
putInput tw (Input tokens) = do
  let n = fromIntegral (length tokens) :: Word64
  putWord64le (8 + n * uintBytes tw)
  putWord64le n
  mapM_ (putUInt tw) tokens

getInput :: UInt -> Get Input
getInput tw = do
  frame_len <- getWord64le
  n <- getWord64le
  when (frame_len /= 8 + n * uintBytes tw) $
    fail "Frame length does not match the payload size."
  Input <$> replicateM (fromIntegral n) (getUInt tw)

putInputs :: UInt -> Inputs -> Put
putInputs tw (Inputs inps) = do
  putWord64le (fromIntegral $ length inps)
  mapM_ (putInput tw) inps

getInputs :: UInt -> Get Inputs
getInputs tw = do
  i <- getWord64le
  Inputs <$> replicateM (fromIntegral i) (getInput tw)

putOutputs :: UInt -> Outputs -> Put
putOutputs pw (Outputs results) = do
  putWord64le (fromIntegral $ length results)
  mapM_ (putOutput pw) results

getOutputs :: UInt -> Get Outputs
getOutputs pw = do
  i <- getWord64le
  Outputs <$> replicateM (fromIntegral i) (getOutput pw)

-- | Generate a parseable token sequence using derivations from the
-- grammar.  Start with the start symbol and randomly choose
-- productions until we derive a string of terminals of the desired
-- length.
generateSingleLongTokenSequence :: (Ord nt, Ord t, Show nt, Show t) => Int -> Grammar (AugmentedNonterminal nt) (AugmentedTerminal t) -> [t]
generateSingleLongTokenSequence len grammar =
  let gen = mkStdGen randomSeed
      -- Get all derivations of length up to len from the start symbol
      -- We use len to ensure we get all derivations up to and including length len
      (_, ts) = generateRandomDerivation gen len grammar
   in mapMaybe unaug ts
  where
    unaug (AugmentedTerminal t) = Just t
    unaug _ = Nothing

parserTests :: TestMode -> CFG -> Int -> Bool -> Either Text (LBS.ByteString, LBS.ByteString)
parserTests mode cfg n noOutputs = do
  let q = paramsLookback $ cfgParams cfg
      k = paramsLookahead $ cfgParams cfg
  grammar <- cfgToGrammar cfg
  table <- llpParserTableWithStarts q k $ getGrammar grammar
  let s_encoder = encodeSymbols (T "ignore") grammar
  tw <- symbolTerminalIntType s_encoder
  pw <- productionIntType grammar
  let p x =
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
                if noOutputs then Outputs [] else Outputs $ Output . fmap (fmap fromIntegral) . parse <$> comb
              )
        SingleLong ->
          let singleSeq = generateSingleLongTokenSequence n (getGrammar grammar)
              comb = [singleSeq]
           in ( Inputs $ Input . fmap (fromIntegral . encode' . AugmentedTerminal) <$> comb,
                if noOutputs then Outputs [] else Outputs $ Output . fmap (fmap fromIntegral) . parse <$> comb
              )
      parse = llpParse q k table
  pure
    ( runPut $ putInputs tw inputs,
      runPut $ putOutputs pw outputs
    )
  where
    unaug (AugmentedTerminal t) = Just t
    unaug _ = Nothing

-- | SingleLong variant that streams the framed payload to the .inputs handle
-- and, when outputs are requested, streams the production list to the .outputs
-- handle.  Writing is done in a single streaming fold; parsing requires a
-- second derivation pass (same seed ⇒ same sequence) so the token list and
-- the write buffer never coexist in memory.
parserTestsSingleLong ::
  CFG ->
  Int ->
  Handle ->       -- ^ .inputs handle (WriteMode)
  Maybe Handle -> -- ^ Just outH to stream .outputs; Nothing when noOutputs
  IO (Either Text ())
parserTestsSingleLong cfg n h mOutH = do
  case cfgToGrammar cfg of
    Left e -> pure (Left e)
    Right grammar ->
      case llpParserTableWithStarts q k $ getGrammar grammar of
        Left e -> pure (Left e)
        Right table -> do
          let s_encoder = encodeSymbols (T "ignore") grammar
          case (,) <$> symbolTerminalIntType s_encoder <*> productionIntType grammar of
            Left e -> pure (Left e)
            Right (tw, pw) -> do
              let encode' x = fromJust $ Terminal x `symbolLookup` s_encoder
                  gen = mkStdGen randomSeed
                  -- Stream-write phase: consume the lazy derivation list one token at a
                  -- time, writing each to h.  GHC reclaims list cells as we go, so the
                  -- live set is O(1).
                  (_, rawLazy) = generateRandomDerivationLazy gen n (getGrammar grammar)
                  lazySeq = mapMaybe unaug rawLazy
              -- num_tests, then frame_len/n placeholders patched below.
              LBS.hPut h (runPut $ putWord64le 1 >> putWord64le 0 >> putWord64le 0)
              totalToks <- foldM (\(!cnt) t -> do
                    let w = fromIntegral (encode' (AugmentedTerminal t)) :: Word64
                    LBS.hPut h (runPut $ putUInt tw w)
                    pure (cnt + 1)) (0 :: Word64) lazySeq
              hSeek h AbsoluteSeek 8
              LBS.hPut h $ runPut $ do
                putWord64le (8 + totalToks * uintBytes tw)
                putWord64le totalToks
              case mOutH of
                Nothing -> pure (Right ())
                Just outH -> do
                  -- Parse phase: re-derive with same seed (same sequence) for llpParse.
                  let (_, rawSeq) = generateRandomDerivation gen n (getGrammar grammar)
                      singleSeq = mapMaybe unaug rawSeq
                      mProds = fmap (fmap fromIntegral) $ llpParse q k table singleSeq
                  case mProds of
                    Nothing -> do
                      LBS.hPut outH (runPut $ putWord64le 1 >> putWord8 0)
                      pure (Right ())
                    Just prods -> do
                      LBS.hPut outH $ runPut $ do
                        putWord64le 1
                        putWord8 1
                        putWord64le (fromIntegral (length prods))
                      mapM_ (\p -> LBS.hPut outH (runPut $ putUInt pw p)) prods
                      pure (Right ())
  where
    q = paramsLookback $ cfgParams cfg
    k = paramsLookahead $ cfgParams cfg
    unaug (AugmentedTerminal t) = Just t
    unaug _ = Nothing

parserTestsCompare :: CFG -> ByteString -> ByteString -> ByteString -> Either Text ()
parserTestsCompare cfg input expected result = do
  grammar <- cfgToGrammar cfg
  let s_encoder = encodeSymbols (T "ignore") grammar
  tw <- symbolTerminalIntType s_encoder
  pw <- productionIntType grammar
  Inputs inp <- decodeWith "Error: Could not parse input file." (getInputs tw) input
  Outputs ex <- decodeWith "Error: Could not parse expected output file." (getOutputs pw) expected
  Outputs res <- decodeWith "Error: Could not parse result output file." (getOutputs pw) result
  failwith (not (null inp)) "Error: Input file contains no tests."
  failwith (length inp == length ex) "Error: Input and expected output file do not have the same number of tests."
  failwith (length inp == length res) "Error: Input and result output file do not have the same number of tests."

  mapM_ compareTest $ zip4 [0 :: Integer ..] inp ex res
  where
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
