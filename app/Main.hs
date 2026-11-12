module Main where

import Alpacc.CFG
import Alpacc.Types
import Alpacc.Generator.Analyzer
  ( Generator (..),
    mkLexer,
    mkLexerParser,
    mkParser,
  )
import Alpacc.Generator.C.Generator qualified as C
import Alpacc.Generator.Cuda.Generator qualified as Cuda
import Alpacc.Generator.Futhark.Generator qualified as Futhark
import Alpacc.Random qualified as Random
import Alpacc.Test
import Alpacc.Test.Lexer (lexerTestsSingleLong)
import Alpacc.Test.LexerParser (lexerParserTestsSingleLong)
import Alpacc.Test.Parser (parserTestsSingleLong)
import CudaProbe qualified
import Control.Monad (unless)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Options.Applicative
import System.Exit (exitFailure)
import System.FilePath.Posix (stripExtension, takeFileName)
import System.IO
import Prelude hiding (last)

data Backend
  = Futhark
  | CUDA
  | C
  deriving (Show)

data Input
  = FileInput !FilePath
  | StdInput
  deriving (Show)

data Gen
  = GenLexer
  | GenParser
  | GenBoth
  deriving (Show)

data Command
  = Generate !GeneratorParameters
  | Test !TestCommand
  | Random !RandomParameters
  deriving (Show)

data TestCommand
  = TestGenerate !TestGenerateParameters
  | TestCompare !TestCompareParameters
  deriving (Show)

combine :: Gen -> Gen -> Gen
a `combine` GenBoth = a
GenBoth `combine` a = a
GenLexer `combine` GenLexer = GenLexer
GenParser `combine` GenParser = GenParser
_ `combine` _ = GenBoth

data GeneratorParameters = GeneratorParameters
  { generatorInput :: !Input,
    generatorOutput :: !(Maybe String),
    generatorGenerator :: !Gen,
    generatorBackend :: !Backend,
    generatorIndex32 :: !Bool,
    generatorSharedMemory :: !(Maybe Int),
    -- ^ Nothing = auto (probe local device); Just n = literal bytes.
    generatorSmArch :: !(Maybe Int)
    -- ^ Nothing = auto (probe local device); Just n = literal compute
    -- capability × 10 (e.g. 75 for sm_75).
  }
  deriving (Show)

data RandomParameters = RandomParameters
  { randomOutput :: !(Maybe String),
    randomNumChars :: !Int,
    randomNumTerminals :: !Int,
    randomNumNonterminals :: !Int,
    randomNumProductions :: !Int
  }
  deriving (Show)

data TestGenerateParameters = TestGenerateParameters
  { testGenerateInput :: !Input,
    testGenerateOutput :: !(Maybe String),
    testGenerateLength :: !Int,
    testGenerateGenerator :: !Gen,
    testGenerateMode :: !TestMode,
    testGenerateNoOutputs :: !Bool,
    testGenerateIndex32 :: !Bool
  }
  deriving (Show)

data TestCompareParameters = TestCompareParameters
  { testCompareCFG :: !Input,
    testCompareInput :: !String,
    testCompareExpected :: !String,
    testCompareResult :: !String,
    testCompareGenerator :: !Gen,
    testCompareIndex32 :: !Bool
  }
  deriving (Show)

lengthParameter :: Parser Int
lengthParameter =
  option
    auto
    ( long "length"
        <> short 'l'
        <> help "The maximum length of inputs (exhaustive mode, default) or exact length (single-long mode)."
        <> showDefault
        <> value 7
        <> metavar "INT"
    )

testModeParameter :: Parser TestMode
testModeParameter =
  flag
    Exhaustive
    SingleLong
    ( long "single-long"
        <> help "Generate a single long test instead of exhaustive tests."
    )

lookbackParameter :: Parser Int
lookbackParameter =
  option
    auto
    ( long "lookback"
        <> short 'q'
        <> help "The amount of characters used for lookback."
        <> showDefault
        <> value 0
        <> metavar "INT"
    )

lexerParameter :: Parser Gen
lexerParameter =
  flag'
    GenLexer
    ( long "lexer"
        <> short 'l'
        <> help "Generate a lexer."
    )
    <|> pure GenBoth

parserParameter :: Parser Gen
parserParameter =
  flag'
    GenParser
    ( long "parser"
        <> short 'p'
        <> help "Generate a parser."
    )
    <|> pure GenBoth

lookaheadParameter :: Parser Int
lookaheadParameter =
  option
    auto
    ( long "lookahead"
        <> short 'k'
        <> help "The amount of characters used for lookahead."
        <> showDefault
        <> value 1
        <> metavar "INT"
    )

numCharsParameter :: Parser Int
numCharsParameter =
  option
    auto
    ( long "num-chars"
        <> short 'c'
        <> help "The amount of different chars, max is 26."
        <> showDefault
        <> value 3
        <> metavar "INT"
    )

numTerminalsParameter :: Parser Int
numTerminalsParameter =
  option
    auto
    ( long "num-terminals"
        <> short 't'
        <> help "The amount of terminals."
        <> showDefault
        <> value 3
        <> metavar "INT"
    )

numNonterminalsParameter :: Parser Int
numNonterminalsParameter =
  option
    auto
    ( long "num-nonterminals"
        <> short 'n'
        <> help "The amount of nonterminals."
        <> showDefault
        <> value 2
        <> metavar "INT"
    )

numProductionsParameter :: Parser Int
numProductionsParameter =
  option
    auto
    ( long "num-productions"
        <> short 'p'
        <> help "The amount of nonterminals."
        <> showDefault
        <> value 3
        <> metavar "INT"
    )

outputParameter :: Parser (Maybe String)
outputParameter =
  optional $
    strOption
      ( long "output"
          <> short 'o'
          <> help "The name of the output file."
          <> metavar "FILE"
      )

fileInput :: Parser Input
fileInput = FileInput <$> argument str (metavar "FILE")

stdInput :: Parser Input
stdInput =
  flag'
    StdInput
    ( long "stdin"
        <> short 's'
        <> help "Read from stdin."
    )

inputParameter :: Parser Input
inputParameter = fileInput <|> stdInput

lexerParserParametar :: Parser Gen
lexerParserParametar = combine <$> parserParameter <*> lexerParameter

generateParametar :: Parser Gen
generateParametar = lexerParserParametar <|> pure GenBoth

generatorParameters :: Backend -> Parser Command
generatorParameters backend =
  Generate
    <$> ( GeneratorParameters
            <$> inputParameter
            <*> outputParameter
            <*> generateParametar
            <*> pure backend
            <*> switch (long "index32" <> help "Use 32-bit integers for indices (Futhark only).")
            <*> optional
                  ( option auto
                      ( long "shared-memory"
                          <> metavar "BYTES"
                          <> help "Per-block shared memory budget baked into the generated CUDA kernel (CUDA backend only).  Default: auto-probe the local device via nvcc.  Overridable at nvcc time with -DALPACC_SHARED_MEMORY=<n>."
                      )
                  )
            <*> optional
                  ( option auto
                      ( long "sm-arch"
                          <> metavar "SM"
                          <> help "Target GPU compute capability × 10 (e.g. 75 for sm_75, 80 for sm_80) baked in as ALPACC_SM_ARCH; the codegen picks BLOCK_SIZE / ITEMS_PER_THREAD from a per-arch tuning table when this is set (CUDA backend only).  Default: auto-probe the local device via nvcc.  Overridable at nvcc time with -DALPACC_SM_ARCH=<n>."
                      )
                  )
        )

randomParameters :: Parser Command
randomParameters =
  Random
    <$> ( RandomParameters
            <$> outputParameter
            <*> numCharsParameter
            <*> numTerminalsParameter
            <*> numNonterminalsParameter
            <*> numProductionsParameter
        )

testGenerateParameters :: Parser Command
testGenerateParameters =
  Test . TestGenerate
    <$> ( TestGenerateParameters
            <$> inputParameter
            <*> outputParameter
            <*> lengthParameter
            <*> generateParametar
            <*> testModeParameter
            <*> switch
              ( long "no-outputs"
                  <> help "Only write the .inputs file, skip generating .outputs."
              )
            <*> switch (long "index32" <> help "Use 32-bit indices in the .outputs wire format (matches the generator's --index32).")
        )

testCompareParameters :: Parser Command
testCompareParameters =
  Test . TestCompare
    <$> ( TestCompareParameters
            <$> inputParameter
            <*> argument str (metavar "FILE")
            <*> argument str (metavar "FILE")
            <*> argument str (metavar "FILE")
            <*> generateParametar
            <*> switch (long "index32" <> help "Interpret the result file with 32-bit indices (matches the generator's --index32).")
        )

testCommands :: Parser Command
testCommands =
  subparser
    ( command "generate" (info testGenerateParameters (progDesc "Generate tests."))
        <> command "compare" (info testCompareParameters (progDesc "Inspect if test passed."))
    )

commands :: Parser Command
commands =
  subparser
    ( command "futhark" (info (generatorParameters Futhark) (progDesc "Generate parsers written in Futhark."))
        <> command "cuda" (info (generatorParameters CUDA) (progDesc "Generate parsers written in CUDA."))
        <> command "c" (info (generatorParameters C) (progDesc "Generate parsers written in C."))
        <> command "random" (info randomParameters (progDesc "Generate random parser that can be used for testing."))
        <> command "test" (info testCommands (progDesc "Test related commands."))
    )

options :: ParserInfo Command
options =
  info
    (commands <**> helper)
    ( fullDesc
        <> progDesc "Creates a parallel parser in Futhark using FILE."
        <> header "Alpacc"
    )

writeProgram :: String -> Text -> IO ()
writeProgram program_path program = do
  TextIO.writeFile program_path program
  putStrLn ("The parser " ++ program_path ++ " was created.")

extension :: Backend -> String
extension backend =
  case backend of
    CUDA -> ".cu"
    Futhark -> ".fut"
    C -> ".c"

outputPath :: Backend -> Maybe String -> Input -> String
outputPath backend output input =
  case output of
    Just path -> path
    Nothing -> case input of
      StdInput -> "parser" ++ ext
      FileInput path ->
        (++ ext)
          . fromJust
          . stripExtension "alp"
          $ takeFileName path
  where
    ext = extension backend

readContents :: Input -> IO Text
readContents input =
  case input of
    StdInput -> TextIO.getContents
    FileInput path -> TextIO.readFile path

bitsToUInt :: Int -> Maybe UInt
bitsToUInt 8  = Just U8
bitsToUInt 16 = Just U16
bitsToUInt 32 = Just U32
bitsToUInt 64 = Just U64
bitsToUInt _  = Nothing

backendGenerator :: Backend -> Bool -> Maybe UInt -> Int -> Int -> Generator [Text]
backendGenerator CUDA    index32 mLenType shmem sm = Cuda.generator index32 mLenType shmem sm
backendGenerator Futhark index32 mLenType _     _  = Futhark.generator index32 mLenType
backendGenerator C       index32 mLenType _     _  = C.generator index32 mLenType

generateProgram :: Backend -> Bool -> Maybe UInt -> Int -> Int -> Gen -> CFG -> Either Text Text
generateProgram backend index32 mLenType shmem sm gen cfg =
  case gen of
    GenBoth -> generate generator <$> mkLexerParser cfg
    GenLexer -> generate generator <$> mkLexer cfg
    GenParser -> generate generator <$> mkParser cfg
  where
    generator = backendGenerator backend index32 mLenType shmem sm

pathOfInput :: FilePath -> Input -> FilePath
pathOfInput p StdInput = p
pathOfInput _ (FileInput p) = p

readCfg :: Input -> IO CFG
readCfg input = do
  contents <- readContents input
  case cfgFromText (pathOfInput "" input) contents of
    Left e -> do
      hPutStrLn stderr $ Text.unpack e
      exitFailure
    Right g -> pure g

eitherToIO :: Either Text a -> IO a
eitherToIO (Left e) = do
  hPutStrLn stderr $ Text.unpack e
  exitFailure
eitherToIO (Right a) = pure a

mainGenerator :: GeneratorParameters -> IO ()
mainGenerator params = do
  let program_path = outputPath backend output input
  cfg <- readCfg input
  let mLenType = paramsLength (cfgParams cfg) >>= bitsToUInt
  (shmem, smArch) <- resolveCudaDeviceParams backend mShmem mSmArch
  let either_program = generateProgram backend index32 mLenType shmem smArch gen cfg

  case either_program of
    Left e -> do
      TextIO.hPutStrLn stderr e
      exitFailure
    Right program -> writeProgram program_path program
  where
    backend = generatorBackend params
    output = generatorOutput params
    input = generatorInput params
    gen = generatorGenerator params
    index32 = generatorIndex32 params
    mShmem = generatorSharedMemory params
    mSmArch = generatorSmArch params

-- | Resolve --shared-memory and --sm-arch when either is left as auto.
-- Only probes when the backend is CUDA (probing for C/Futhark is waste).
-- Fails hard with a clear message when nvcc/GPU is unavailable and one of
-- them is auto — the user can then rerun with explicit values.
resolveCudaDeviceParams :: Backend -> Maybe Int -> Maybe Int -> IO (Int, Int)
resolveCudaDeviceParams CUDA (Just s) (Just a) = pure (s, a)
resolveCudaDeviceParams CUDA mShmem mSmArch = do
  hPutStrLn stderr
    "Probing local GPU via nvcc for --shared-memory / --sm-arch auto-defaults..."
  eR <- CudaProbe.probeDevice
  case eR of
    Right r -> do
      hPutStrLn stderr $
        "  probe: sm_arch=" <> show (CudaProbe.probeSmArch r) <>
        ", shared_memory=" <> show (CudaProbe.probeSharedMemory r) <> " bytes"
      pure ( fromMaybe (CudaProbe.probeSharedMemory r) mShmem
           , fromMaybe (CudaProbe.probeSmArch r)       mSmArch
           )
    Left err -> do
      hPutStrLn stderr $
        "error: --shared-memory / --sm-arch auto-probe failed:\n  " <>
        probeErrMsg err <>
        "\nPass explicit values, e.g. `alpacc cuda --shared-memory 49152 --sm-arch 75 ...`."
      exitFailure
  where
    probeErrMsg (CudaProbe.ProbeNvccMissing s)    = s
    probeErrMsg (CudaProbe.ProbeCompileFailed s)  = s
    probeErrMsg (CudaProbe.ProbeRunFailed s)      = s
    probeErrMsg (CudaProbe.ProbeParseFailed s)    = s
-- Non-CUDA backends never need these values; use portable placeholders.
resolveCudaDeviceParams _ mShmem mSmArch =
  pure (fromMaybe 49152 mShmem, fromMaybe 75 mSmArch)

mainRandom :: RandomParameters -> IO ()
mainRandom params =
  Random.random num_chars num_terminals num_nonterminals num_productions
    >>= writeProgram path
  where
    num_chars = randomNumChars params
    num_terminals = randomNumTerminals params
    num_nonterminals = randomNumNonterminals params
    num_productions = randomNumProductions params
    path = fromMaybe "random.alp" $ randomOutput params

mainTestGenerate :: TestGenerateParameters -> IO ()
mainTestGenerate params = do
  cfg <- readCfg input
  let name =
        case out of
          Just a -> a
          Nothing ->
            fromJust $
              stripExtension "alp" $
                takeFileName $
                  pathOfInput "test.alp" input

  case testGenerateGenerator params of
    GenLexer -> case mode of
      Exhaustive -> do
        (inputs, ouputs) <- eitherToIO $ lexerTests mode cfg len noOutputs index32
        LBS.writeFile (name <> ".inputs") inputs
        unless noOutputs $ LBS.writeFile (name <> ".outputs") ouputs
      SingleLong -> do
        let inputsFile = name <> ".inputs"
        h <- openBinaryFile inputsFile ReadWriteMode
        mOutH <- if noOutputs
                   then pure Nothing
                   else Just <$> openBinaryFile (name <> ".outputs") WriteMode
        result <- lexerTestsSingleLong cfg len index32 h mOutH
        hClose h
        mapM_ hClose mOutH
        eitherToIO result
    GenParser -> case mode of
      Exhaustive -> do
        (inputs, ouputs) <- eitherToIO $ parserTests mode cfg len noOutputs
        LBS.writeFile (name <> ".inputs") inputs
        unless noOutputs $ LBS.writeFile (name <> ".outputs") ouputs
      SingleLong -> do
        let inputsFile = name <> ".inputs"
        h <- openBinaryFile inputsFile WriteMode
        mOutH <- if noOutputs
                   then pure Nothing
                   else Just <$> openBinaryFile (name <> ".outputs") WriteMode
        result <- parserTestsSingleLong cfg len h mOutH
        hClose h
        mapM_ hClose mOutH
        eitherToIO result
    GenBoth -> case mode of
      Exhaustive -> do
        (inputs, ouputs) <- eitherToIO $ lexerParserTests mode cfg len noOutputs index32
        LBS.writeFile (name <> ".inputs") inputs
        unless noOutputs $ LBS.writeFile (name <> ".outputs") ouputs
      SingleLong -> do
        let inputsFile = name <> ".inputs"
            outputsFile = name <> ".outputs"
        h <- openBinaryFile inputsFile ReadWriteMode
        mOutH <- if noOutputs
                   then pure Nothing
                   else Just . (outputsFile,) <$> openBinaryFile outputsFile WriteMode
        result <- lexerParserTestsSingleLong cfg len index32 h mOutH
        hClose h
        mapM_ (hClose . snd) mOutH
        eitherToIO result
  where
    out = testGenerateOutput params
    input = testGenerateInput params
    len = testGenerateLength params
    mode = testGenerateMode params
    noOutputs = testGenerateNoOutputs params
    index32 = testGenerateIndex32 params

mainTestCompare :: TestCompareParameters -> IO ()
mainTestCompare params = do
  cfg <- readCfg input'
  input_bytes <- ByteString.readFile input
  expected_bytes <- ByteString.readFile expected
  result_bytes <- ByteString.readFile result

  case testCompareGenerator params of
    GenLexer -> do
      () <- eitherToIO $ lexerTestsCompare cfg index32 input_bytes expected_bytes result_bytes
      putStrLn "Tests passes."
      pure ()
    GenParser -> do
      () <- eitherToIO $ parserTestsCompare cfg input_bytes expected_bytes result_bytes
      putStrLn "Tests passes."
      pure ()
    GenBoth -> do
      () <- eitherToIO $ lexerParserTestsCompare cfg index32 input_bytes expected_bytes result_bytes
      putStrLn "Tests passes."
      pure ()
  where
    input' = testCompareCFG params
    input = testCompareInput params
    expected = testCompareExpected params
    result = testCompareResult params
    index32 = testCompareIndex32 params

main :: IO ()
main = do
  opts <- execParser options
  case opts of
    Generate params -> mainGenerator params
    Random params -> mainRandom params
    Test test -> case test of
      TestGenerate params -> mainTestGenerate params
      TestCompare params -> mainTestCompare params
