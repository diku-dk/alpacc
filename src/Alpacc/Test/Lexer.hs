module Alpacc.Test.Lexer
  ( lexerTests,
    lexerTestsSingleLong,
    lexerTestsCompare,
    lexerBytes,
    TestMode (..),
    randomSeed,
    putUInt,
    getUInt,
    uintBytes,
    decodeWith,
    cfgLengthType,
    indexType,
    putSInt,
    getSInt,
    sintBytes,
  )
where

import Alpacc.CFG
import Alpacc.Encode
import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Alpacc.Lexer.FSA
import Alpacc.Lexer.RegularExpression
import Alpacc.Types
import Alpacc.Util
import Control.Monad
import Data.Bifunctor
import Data.IORef
import Data.Binary.Get (Get, getByteString, getWord16le, getWord32le, getWord64le, getWord8, runGetOrFail)
import Data.Binary.Put (Put, putByteString, putWord16le, putWord32le, putWord64le, putWord8, runPut)
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal
import Data.ByteString.Lazy qualified as LBS
import Data.Either.Extra
import Data.Array.IArray (Array, accumArray, bounds, listArray, (!))
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray)
import Data.List (zip4)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Int (Int32, Int64)
import Data.Word (Word8, Word32, Word64)
import System.IO (Handle, SeekMode (..), hFlush, hSeek, hSetFileSize, hTell)
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

-- | Little-endian putter for a value of the given unsigned native width.
putUInt :: UInt -> Word64 -> Put
putUInt U8 = putWord8 . fromIntegral
putUInt U16 = putWord16le . fromIntegral
putUInt U32 = putWord32le . fromIntegral
putUInt U64 = putWord64le

-- | Little-endian getter for a value of the given unsigned native width.
getUInt :: UInt -> Get Word64
getUInt U8 = fromIntegral <$> getWord8
getUInt U16 = fromIntegral <$> getWord16le
getUInt U32 = fromIntegral <$> getWord32le
getUInt U64 = getWord64le

uintBytes :: UInt -> Word64
uintBytes = fromIntegral . (`div` 8) . numBits

-- | Little-endian, two's-complement putter for wire @index_t@ (signed).
--   Only 32-bit and 64-bit widths are supported by the wire format.
putSInt :: IInt -> Int64 -> Put
putSInt I32 = putWord32le . fromIntegral . (fromIntegral :: Int64 -> Int32)
putSInt I64 = putWord64le . fromIntegral
putSInt w   = error $ "putSInt: unsupported wire width " <> show w

-- | Little-endian, two's-complement getter for wire @index_t@ (signed).
getSInt :: IInt -> Get Int64
getSInt I32 = fromIntegral . (fromIntegral :: Word32 -> Int32) <$> getWord32le
getSInt I64 = (fromIntegral :: Word64 -> Int64) <$> getWord64le
getSInt w   = error $ "getSInt: unsupported wire width " <> show w

sintBytes :: IInt -> Word64
sintBytes I32 = 4
sintBytes I64 = 8
sintBytes w   = error $ "sintBytes: unsupported wire width " <> show w

decodeWith :: Text -> Get a -> ByteString -> Either Text a
decodeWith err g =
  bimap (const err) (\(_, _, a) -> a)
    . runGetOrFail g
    . LBS.fromStrict

-- | Derive the length-field type from a CFG's params. When no explicit
-- `length` param is set, defaults to the unsigned counterpart of index_t
-- (U32 when --index32, else U64), matching the wire-protocol spec and
-- the CUDA/Futhark generator defaults.
cfgLengthType :: Bool -> CFG -> UInt
cfgLengthType index32 cfg = case paramsLength (cfgParams cfg) of
  Just 8  -> U8
  Just 16 -> U16
  Just 32 -> U32
  Just 64 -> U64
  _       -> if index32 then U32 else U64

-- | Wire width of index_t for a given --index32 setting.
indexType :: Bool -> IInt
indexType True  = I32
indexType False = I64

putOutput :: UInt -> IInt -> UInt -> Output -> Put
putOutput _ _ _ (Output Nothing) = putWord8 0
putOutput tw iw lw (Output (Just ts)) = do
  putWord8 1
  putWord64le (fromIntegral $ length ts)
  mapM_ putToken ts
  where
    putToken (Lexeme t (i, j)) = do
      putUInt tw t
      putSInt iw (fromIntegral i)
      putUInt lw (j - i)

getOutput :: UInt -> IInt -> UInt -> Get Output
getOutput tw iw lw = do
  is_valid <- getWord8
  if is_valid == 1
    then do
      num_tokens <- getWord64le
      Output . Just <$> replicateM (fromIntegral num_tokens) getLexeme
    else pure $ Output Nothing
  where
    getLexeme = do
      t <- getUInt tw
      i <- fromIntegral <$> getSInt iw
      len <- getUInt lw
      pure $ Lexeme t (i, i + len)

putInput :: Input -> Put
putInput (Input str) = do
  let n = fromIntegral (ByteString.length str) :: Word64
  putWord64le (8 + n)
  putWord64le n
  putByteString str

getInput :: Get Input
getInput = do
  frame_len <- getWord64le
  n <- getWord64le
  when (frame_len /= 8 + n) $
    fail "Frame length does not match the payload size."
  Input <$> getByteString (fromIntegral n)

putInputs :: Inputs -> Put
putInputs (Inputs inps) = do
  putWord64le (fromIntegral $ length inps)
  mapM_ putInput inps

getInputs :: Get Inputs
getInputs = do
  i <- getWord64le
  Inputs <$> replicateM (fromIntegral i) getInput

putOutputs :: UInt -> IInt -> UInt -> Outputs -> Put
putOutputs tw iw lw (Outputs results) = do
  putWord64le (fromIntegral $ length results)
  mapM_ (putOutput tw iw lw) results

getOutputs :: UInt -> IInt -> UInt -> Get Outputs
getOutputs tw iw lw = do
  i <- getWord64le
  Outputs <$> replicateM (fromIntegral i) (getOutput tw iw lw)

-- | Precomputed dense-indexed DFA structures for the token-targeted walk.
-- States are renumbered to dense Ints so the walk loop uses O(1) array
-- indexing.
data WalkEnv = WalkEnv
  { weInitial :: Int,
    weNumStates :: Int,
    weProducing :: Array Int [(Word8, Int)],
    weNonProducing :: Array Int [(Word8, Int)],
    weRevNonProducing :: Array Int [Int],
    weAccepting :: [Int]
  }

mkWalkEnv :: (Ord s, Ord t) => DFALexer Word8 s t -> WalkEnv
mkWalkEnv dfa_lexer =
  WalkEnv
    { weInitial = ix (initial dfa),
      weNumStates = n,
      weProducing = accumArray (flip (:)) [] (0, n - 1) producing,
      weNonProducing = accumArray (flip (:)) [] (0, n - 1) non_producing,
      weRevNonProducing = accumArray (flip (:)) [] (0, n - 1) rev_non_producing,
      weAccepting = [ix s | s <- Set.toList (accepting dfa)]
    }
  where
    dfa = fsa dfa_lexer
    trs = Map.toList $ transitions' dfa
    prod = producesToken dfa_lexer
    st_idx = Map.fromList $ zip (Set.toList (states dfa)) [0 ..]
    n = Map.size st_idx
    ix s = st_idx Map.! s
    producing =
      [(ix s, (sym, ix s')) | ((s, sym), s') <- trs, (s, sym) `Set.member` prod]
    non_producing =
      [(ix s, (sym, ix s')) | ((s, sym), s') <- trs, not ((s, sym) `Set.member` prod)]
    rev_non_producing =
      [(ix s', ix s) | ((s, sym), s') <- trs, not ((s, sym) `Set.member` prod)]

-- | Distance-to-target and live-transition arrays for a walk that must reach
-- one of the target accepting states along non-producing transitions.
data TokenTarget = TokenTarget
  { ttDist :: UArray Int Int,
    ttAllowed :: Array Int (Array Int (Word8, Int)),
    ttEntry :: Array Int (Array Int (Word8, Int)),
    ttStep :: Array Int (Maybe (Word8, Int))
  }

-- | Multi-source BFS from the target states along reversed non-producing
-- transitions.
bfsDist :: Int -> Array Int [Int] -> [Int] -> UArray Int Int
bfsDist n rev targets = runSTUArray $ do
  dist <- newArray (0, n - 1) (-1)
  mapM_ (\s -> writeArray dist s 0) targets
  let expand d acc s =
        foldM
          ( \acc' p -> do
              dp <- readArray dist p
              if dp < 0
                then writeArray dist p (d + 1) >> pure (p : acc')
                else pure acc'
          )
          acc
          (rev ! s)
      go _ [] = pure ()
      go d frontier = go (d + 1) =<< foldM (expand d) [] frontier
  go (0 :: Int) targets
  pure dist

mkTokenTarget :: WalkEnv -> [Int] -> TokenTarget
mkTokenTarget env targets =
  TokenTarget
    { ttDist = dist,
      ttAllowed = filterLive (weNonProducing env),
      ttEntry = filterLive (weProducing env),
      ttStep = step
    }
  where
    n = weNumStates env
    dist = bfsDist n (weRevNonProducing env) targets
    filterLive edges =
      listArray
        (0, n - 1)
        [ let xs = [t | t@(_, s') <- edges ! s, dist ! s' >= 0]
           in listArray (0, length xs - 1) xs
        | s <- [0 .. n - 1]
        ]
    step =
      listArray
        (0, n - 1)
        [ listToMaybe
            [t | t@(_, s') <- weNonProducing env ! s, dist ! s' == dist ! s - 1]
        | s <- [0 .. n - 1]
        ]

data WalkMode = WalkRandom | WalkShortest

data TokenWalk g = TokenWalk
  { twBytes :: [Word8],
    twEnd :: !Int,
    twGen :: !g
  }

-- | Walk one token, starting just after the entry byte.  The walk only
-- takes allowed (live) transitions, so it can always still reach a target
-- accepting state.  At target states it stops with probability increasing
-- in the number of steps taken; once the per-token byte budget is reached
-- it follows a shortest path to the nearest target state, so token lengths
-- stay bounded by budget + max shortest-path distance in the DFA.
walkToken :: (RandomGen g) => TokenTarget -> Int -> WalkMode -> Int -> g -> TokenWalk g
walkToken tt budget mode s0 g0 =
  case mode of
    WalkRandom -> go g0 s0 1 []
    WalkShortest ->
      let (bs, s_end) = converge s0 []
       in TokenWalk {twBytes = bs, twEnd = s_end, twGen = g0}
  where
    dist s = ttDist tt ! s
    converge s acc
      | dist s <= 0 = (reverse acc, s)
      | otherwise =
          case ttStep tt ! s of
            Just (b, s') -> converge s' (b : acc)
            Nothing -> (reverse acc, s)
    stop g s acc = TokenWalk {twBytes = reverse acc, twEnd = s, twGen = g}
    go g s m acc
      | m >= budget =
          let (bs, s_end) = converge s acc
           in TokenWalk {twBytes = bs, twEnd = s_end, twGen = g}
      | dist s == 0 =
          let (r, g') = randomR (0, budget - 1) g
           in if r < m then stop g' s acc else step g' s m acc
      | otherwise = step g s m acc
    step g s m acc =
      let arr = ttAllowed tt ! s
          k = snd (bounds arr) + 1
       in if k == 0
            then stop g s acc
            else
              let (i, g') = randomR (0, k - 1) g
                  (b, s') = arr ! i
               in go g' s' (m + 1) (b : acc)

-- | Generate a single long input by simulating the DFA.  The walk targets
-- the union of accepting states, entering each new token via a producing
-- transition and, within a token, restricting to transitions that keep a
-- target accepting state reachable.  Token boundaries are correct by
-- construction and token length is bounded: past the (optional) per-token
-- budget the walk converges to the nearest accepting state along the
-- shortest path.
generateSingleLongInputFromDFA :: (Ord s, Ord t) => Maybe Int -> Int -> Set.Set Word8 -> DFALexer Word8 s t -> [Word8]
generateSingleLongInputFromDFA mBudget len alpha dfa_lexer =
  let env = mkWalkEnv dfa_lexer
      tt = mkTokenTarget env (weAccepting env)
      budget = case mBudget of
        Just b | b > 0 -> b
        _ -> max 1 len
      gen = mkStdGen randomSeed
      alphaList = Set.toList alpha
      alphaArr :: Array Int Word8
      alphaArr = listArray (0, length alphaList - 1) alphaList

      -- Pick an entry byte and initial state for the next token.
      pickEntry g start =
        let cands = case start of
              Nothing -> ttAllowed tt ! weInitial env
              Just s -> ttEntry tt ! s
            k = snd (bounds cands) + 1
         in if k == 0
              then Nothing
              else
                let (i, g') = randomR (0, k - 1) g
                 in Just (cands ! i, g')

      -- Emit tokens until we hit the target length.
      loop g written start acc
        | written >= len = reverse acc
        | otherwise =
            case pickEntry g start of
              Nothing -> reverse acc
              Just ((b0, s1), g') ->
                let walk = walkToken tt budget WalkRandom s1 g'
                    bytes = b0 : twBytes walk
                    written' = written + length bytes
                 in loop (twGen walk) written' (Just (twEnd walk)) (foldl (flip (:)) acc bytes)

      result = loop gen 0 Nothing []
      fallback =
        let numChoices = length alphaList
            randomIndices = take len $ randomRs (0, numChoices - 1) gen
         in map (alphaArr !) randomIndices
   in if null result then fallback else take len result

lexerTests :: TestMode -> CFG -> Int -> Bool -> Bool -> Either Text (LBS.ByteString, LBS.ByteString)
lexerTests mode cfg k noOutputs index32 = do
  spec <- cfgToDFALexerSpec cfg
  let ts = Map.keys $ regexMap spec
      encoder = encodeTerminals (T "ignore") $ parsingTerminals ts
      lw = cfgLengthType index32 cfg
      iw = indexType index32
  tw <- terminalIntType encoder
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
           in (toInputs comb, if noOutputs then emptyOutputs else toOutputs dfa ignore comb)
        SingleLong ->
          let singleInput = generateSingleLongInputFromDFA (Just 16) k alpha dfa
           in (toInputs [singleInput], if noOutputs then emptyOutputs else toOutputs dfa ignore [singleInput])
  pure
    ( runPut $ putInputs inputs,
      runPut $ putOutputs tw iw lw outputs
    )
  where
    toOutputs dfa ignore = Outputs . fmap (Output . tokenize dfa ignore)
    toInputs = Inputs . fmap (Input . ByteString.pack)
    emptyOutputs = Outputs []

-- | SingleLong variant that streams the framed payload to the .inputs handle
-- and, when outputs are requested, streams the lexer output to the .outputs
-- handle — the payload is never fully held in memory as a list.
lexerTestsSingleLong ::
  CFG ->
  Int ->
  Bool ->         -- ^ True = index_t is int32_t (--index32)
  Handle ->       -- ^ .inputs handle (ReadWriteMode for seek-back)
  Maybe Handle -> -- ^ Just outH to stream .outputs; Nothing when noOutputs
  IO (Either Text ())
lexerTestsSingleLong cfg k index32 h mOutH = do
  case cfgToDFALexerSpec cfg of
    Left e -> pure (Left e)
    Right spec -> do
      let ts = Map.keys $ regexMap spec
          encoder = encodeTerminals (T "ignore") $ parsingTerminals ts
      case terminalIntType encoder of
        Left e -> pure (Left e)
        Right tw ->
          case mapTokens (fmap (fromIntegral :: Integer -> Word64) . (`terminalLookup` encoder)) $
                 lexerDFA (0 :: Integer) $ mapSymbols unBytes spec of
            Nothing -> pure (Left "Error: Could not encode tokens.")
            Just dfa -> do
              let lw     = cfgLengthType index32 cfg
                  iw     = indexType index32
                  ignore = fromIntegral <$> terminalLookup (T "ignore") encoder
                  alpha  = alphabet $ fsa dfa
                  singleInput = generateSingleLongInputFromDFA (Just 16) k alpha dfa
              -- num_tests, then frame_len/n placeholders patched below.
              LBS.hPut h (runPut $ putWord64le 1 >> putWord64le 0 >> putWord64le 0)
              LBS.hPut h (LBS.pack singleInput)
              let payloadLen = fromIntegral (length singleInput) :: Word64
              hSeek h AbsoluteSeek 8
              LBS.hPut h (runPut $ putWord64le (8 + payloadLen) >> putWord64le payloadLen)
              case mOutH of
                Nothing -> pure (Right ())
                Just outH -> do
                  -- Read back the payload as a compact strict ByteString and tokenize.
                  hSeek h AbsoluteSeek 24
                  payload <- ByteString.hGet h (fromIntegral payloadLen)
                  -- Outputs header: num_tests=1, then valid/count placeholder.
                  LBS.hPut outH (runPut $ putWord64le 1)
                  validPos <- hTell outH
                  LBS.hPut outH (runPut $ putWord8 0 >> putWord64le 0)
                  tokCountRef <- newIORef (0 :: Word64)
                  let putLexeme (Lexeme t (i, j)) = do
                        LBS.hPut outH $ runPut $ do
                          putUInt tw t
                          putSInt iw (fromIntegral i)
                          putUInt lw (j - i)
                        modifyIORef' tokCountRef (+ 1)
                  mResult <- tokenizeWithBS dfa ignore payload putLexeme
                  case mResult of
                    Nothing -> do
                      -- An invalid record is a single 0 byte; drop the count
                      -- placeholder and any partially written lexemes.
                      hFlush outH
                      hSetFileSize outH (validPos + 1)
                      pure (Right ())
                    Just () -> do
                      tokCount <- readIORef tokCountRef
                      hSeek outH AbsoluteSeek validPos
                      LBS.hPut outH (runPut $ putWord8 1 >> putWord64le tokCount)
                      pure (Right ())

-- | Generate raw bytes for a single long lexer input, suitable for piping
-- directly into a lexer benchmark (no binary framing).
lexerBytes :: CFG -> Int -> Either Text ByteString
lexerBytes cfg k = do
  spec <- cfgToDFALexerSpec cfg
  let dfa = lexerDFA (0 :: Integer) $ mapSymbols unBytes spec
      alpha = alphabet $ fsa dfa
  pure $ ByteString.pack $ generateSingleLongInputFromDFA (Just 16) k alpha dfa

lexerTestsCompare :: CFG -> Bool -> ByteString -> ByteString -> ByteString -> Either Text ()
lexerTestsCompare cfg index32 input expected result = do
  spec <- cfgToDFALexerSpec cfg

  let ts = Map.keys $ regexMap spec
      encoder = encodeTerminals (T "ignore") $ parsingTerminals ts
      lw = cfgLengthType index32 cfg
      iw = indexType index32
  tw <- terminalIntType encoder
  encodings <-
    maybeToEither "Error: Could not encode tokens." $
      mapM (fmap fromIntegral . (`terminalLookup` encoder)) ts
  let int_to_token = Map.fromList $ zip encodings ts
  Inputs inp <- decodeWith "Error: Could not parse input file." getInputs input
  Outputs ex <- decodeWith "Error: Could not parse expected output file." (getOutputs tw iw lw) expected
  Outputs res <- decodeWith "Error: Could not parse result output file." (getOutputs tw iw lw) result
  failwith (not (null inp)) "Error: Input file contains no tests."
  failwith (length inp == length ex) "Error: Input and expected output file do not have the same number of tests."
  failwith (length inp == length res) "Error: Input and result output file do not have the same number of tests."

  mapM_ (compareTest int_to_token) $ zip4 [0 :: Integer ..] inp ex res
  where
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
