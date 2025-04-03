module Main where

import Alpacc.CFG
import Alpacc.Generator.Cuda.Generator qualified as Cuda
import Alpacc.Generator.Futhark.Generator qualified as Futhark
import Alpacc.Generator.Generator (Generator (..))
import Data.Maybe
import Data.Text (Text)
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

combine :: Gen -> Gen -> Gen
a `combine` GenBoth = a
GenBoth `combine` a = a
GenLexer `combine` GenLexer = GenLexer
GenParser `combine` GenParser = GenParser
_ `combine` _ = GenBoth

data Parameters = Parameters
  { input :: !Input,
    output :: !(Maybe String),
    lookback :: !Int,
    lookahead :: !Int,
    generator :: !Gen,
    backend :: !Backend
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
        <> value 1
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

parameters :: Backend -> Parser Parameters
parameters backend =
  Parameters
    <$> inputParameter
    <*> outputParameter
    <*> lookbackParameter
    <*> lookaheadParameter
    <*> generateParametar
    <*> pure backend

commands :: Parser Parameters
commands =
  subparser
    ( command "futhark" (info (parameters Futhark) (progDesc "Generate parsers written in CUDA."))
        <> command "cuda" (info (parameters CUDA) (progDesc "Generate parsers written in Futhark."))
    )

options :: ParserInfo Parameters
options =
  info
    (commands <**> helper)
    ( fullDesc
        <> progDesc "Creates a parallel parser in Futhark using FILE."
        <> header "Alpacc"
    )

writeProgram :: String -> String -> IO ()
writeProgram program_path program = do
  writeFile program_path program
  putStrLn ("The parser " ++ program_path ++ " was created.")

extension :: Parameters -> String
extension opts =
  case backend opts of
    CUDA -> ".cu"
    Futhark -> ".fut"

outputPath :: Parameters -> String
outputPath opts =
  case output opts of
    Just path -> path
    Nothing -> case input opts of
      StdInput -> "parser" ++ ext
      FileInput path ->
        (++ ext)
          . fromJust
          . stripExtension "alp"
          $ takeFileName path
  where
    ext = extension opts

readContents :: Parameters -> IO Text
readContents opts =
  case input opts of
    StdInput -> TextIO.getContents
    FileInput path -> TextIO.readFile path

generateProgram :: Parameters -> CFG -> Either String String
generateProgram opts cfg =
  case generator opts of
    GenBoth -> lexerParserGenerator gen q k cfg
    GenLexer -> lexerGenerator gen cfg
    GenParser -> parserGenerator gen q k cfg
  where
    q = lookback opts
    k = lookahead opts
    gen =
      case backend opts of
        CUDA -> Cuda.generator
        Futhark -> Futhark.generator

readCfg :: Parameters -> String -> IO CFG
readCfg opts program_path = do
  contents <- readContents opts
  case cfgFromText program_path contents of
    Left e -> do
      hPutStrLn stderr e
      exitFailure
    Right g -> pure g

main :: IO ()
main = do
  opts <- execParser options
  let program_path = outputPath opts
  cfg <- readCfg opts program_path
  let either_program = generateProgram opts cfg

  case either_program of
    Left e -> do
      hPutStrLn stderr e
      exitFailure
    Right program -> writeProgram program_path program
