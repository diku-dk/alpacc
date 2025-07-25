module Main where

import Alpacc.CFG
import Alpacc.Generator.Analyzer
  ( Generator (..),
    mkLexer,
    mkLexerParser,
    mkParser,
  )
import Alpacc.Generator.Cuda.Generator qualified as Cuda
import Alpacc.Generator.Futhark.Generator qualified as Futhark
import Alpacc.Random qualified as Random
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
  | Test !TestParameters
  | Random !RandomParameters
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
    generatorLookback :: !Int,
    generatorLookahead :: !Int,
    generatorGenerator :: !Gen,
    generatorBackend :: !Backend
  }
  deriving (Show)

data RandomParameters = RandomParameters
  { randomOutput :: !(Maybe String),
    randomLookback :: !Int,
    randomLookahead :: !Int,
    randomGenerator :: !Gen
  }
  deriving (Show)

data TestParameters = TestParameters
  { testInput :: !Input,
    testOutput :: !(Maybe String),
    testLookback :: !Int,
    testLookahead :: !Int,
    testGenerator :: !Gen
  }
  deriving (Show)

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
            <*> lookbackParameter
            <*> lookaheadParameter
            <*> generateParametar
            <*> pure backend
        )

randomParameters :: Parser Command
randomParameters =
  Random
    <$> ( RandomParameters
            <$> outputParameter
            <*> lookbackParameter
            <*> lookaheadParameter
            <*> generateParametar
        )

testParameters :: Parser Command
testParameters =
  Test
    <$> ( TestParameters
            <$> inputParameter
            <*> outputParameter
            <*> lookbackParameter
            <*> lookaheadParameter
            <*> generateParametar
        )

commands :: Parser Command
commands =
  subparser
    ( command "futhark" (info (generatorParameters Futhark) (progDesc "Generate parsers written in CUDA."))
        <> command "cuda" (info (generatorParameters CUDA) (progDesc "Generate parsers written in Futhark."))
        <> command "random" (info randomParameters (progDesc "Generate random parser that can be used for testing."))
        <> command "test" (info testParameters (progDesc "Generate test inputs for ."))
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

generateProgram :: Backend -> Gen -> Int -> Int -> CFG -> Either Text Text
generateProgram backend generator q k cfg =
  case generator of
    GenBoth -> generate gen <$> mkLexerParser q k cfg
    GenLexer -> generate gen <$> mkLexer cfg
    GenParser -> generate gen <$> mkParser q k cfg
  where
    gen =
      case backend of
        CUDA -> Cuda.generator
        Futhark -> Futhark.generator

readCfg :: Input -> String -> IO CFG
readCfg input program_path = do
  contents <- readContents input
  case cfgFromText program_path contents of
    Left e -> do
      hPutStrLn stderr $ Text.unpack e
      exitFailure
    Right g -> pure g

mainGenerator :: GeneratorParameters -> IO ()
mainGenerator params = do
  let program_path = outputPath backend output input
  cfg <- readCfg input program_path
  let either_program = generateProgram backend gen q k cfg

  case either_program of
    Left e -> do
      TextIO.hPutStrLn stderr e
      exitFailure
    Right program -> writeProgram program_path program
  where
    backend = generatorBackend params
    q = generatorLookback params
    k = generatorLookahead params
    output = generatorOutput params
    input = generatorInput params
    gen = generatorGenerator params

mainRandom :: RandomParameters -> IO ()
mainRandom params =
  case gen of
    GenLexer -> Random.randomLexer >>= writeProgram output
    _ -> undefined
  where
    output = fromMaybe "random.alp" $ randomOutput params
    gen = randomGenerator params

main :: IO ()
main = do
  opts <- execParser options
  case opts of
    Generate params -> mainGenerator params
    Random params -> mainRandom params
    Test _ -> undefined
