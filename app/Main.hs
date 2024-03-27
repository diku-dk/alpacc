module Main where

import qualified Data.Text.IO as TextIO
import System.IO
import Alpacc.Generator.Futhark.Generator
import Prelude hiding (last)
import Data.Maybe
import Options.Applicative
import System.FilePath.Posix (stripExtension, takeFileName)
import System.Exit (exitFailure)
import Alpacc.CFG

data Input
  = FileInput FilePath
  | StdInput deriving (Show)

data Generator
  = GenerateLexer
  | GenerateParser
  | GenerateBoth deriving (Show)

instance Semigroup Generator where
  a <> GenerateBoth = a
  GenerateBoth <> a = a
  GenerateLexer <> _ = GenerateBoth
  _ <> GenerateParser = GenerateBoth
  GenerateParser <> _ = GenerateBoth

data Parameters = Parameters
  { input     :: Input
  , output    :: Maybe String
  , lookback  :: Int
  , lookahead :: Int
  , generator :: Generator} deriving (Show)

lookbackParameter :: Parser Int
lookbackParameter =
    option auto
      ( long "lookback"
    <> short 'q'
    <> help "The amount of characters used for lookback."
    <> showDefault
    <> value 1
    <> metavar "INT" )

lexerParameter :: Parser Generator
lexerParameter =
    flag' GenerateLexer
      ( long "lexer"
    <> short 'l'
    <> help "Generate a lexer.")
    <|> pure GenerateBoth

parserParameter :: Parser Generator
parserParameter =
    flag' GenerateParser
      ( long "parser"
    <> short 'p'
    <> help "Generate a parser.")
    <|> pure GenerateBoth

lookaheadParameter :: Parser Int
lookaheadParameter =
    option auto
      ( long "lookahead"
    <> short 'k'
    <> help "The amount of characters used for lookahead."
    <> showDefault
    <> value 1
    <> metavar "INT")

outputParameter :: Parser (Maybe String)
outputParameter =
    optional $ strOption
      ( long "output"
    <> short 'o'
    <> help "The name of the output file."
    <> metavar "FILE" )

fileInput :: Parser Input
fileInput = FileInput <$> argument str (metavar "FILE")

stdInput :: Parser Input
stdInput =
    flag' StdInput
      ( long "stdin"
    <> short 's'
    <> help "Read from stdin.")

inputParameter :: Parser Input
inputParameter = fileInput <|> stdInput

lexerParserParametar :: Parser Generator
lexerParserParametar = liftA2 (<>) parserParameter lexerParameter

generateParametar :: Parser Generator
generateParametar = lexerParserParametar <|> pure GenerateBoth

parameters :: Parser Parameters
parameters = Parameters
  <$> inputParameter
  <*> outputParameter
  <*> lookbackParameter
  <*> lookaheadParameter
  <*> generateParametar

options :: ParserInfo Parameters
options = info (parameters <**> helper)
  ( fullDesc
  <> progDesc "Creates a parallel parser in Futhark using FILE."
  <> header "Alpacc" )

writeFutharkProgram :: String -> String -> IO ()
writeFutharkProgram program_path program = do
  writeFile program_path program
  putStrLn ("The parser " ++ program_path ++ " was created.")

main :: IO ()
main = do
  _options <- execParser options
  let input_method = input _options
  let q = lookback _options
  let k = lookahead _options
  let outfile = output _options
  let gen = generator _options

  let program_path =
        case outfile of
          Just path -> path
          Nothing -> case input_method of
            StdInput -> "parser.fut"
            FileInput path -> (++".fut") . fromJust . stripExtension "cg" $ takeFileName path

  contents <- case input_method of
        StdInput -> TextIO.getContents
        FileInput path -> TextIO.readFile path

  cfg <-
    case cfgFromText program_path contents of
        Left e -> do hPutStrLn stderr e
                     exitFailure
        Right g -> pure g
  
  let either_program =
        case gen of
          GenerateBoth -> generate q k cfg
          GenerateLexer -> generateLexer cfg
          GenerateParser -> generateParser q k cfg

  case either_program of
    Left e -> do hPutStrLn stderr e
                 exitFailure
    Right program -> writeFutharkProgram program_path program
