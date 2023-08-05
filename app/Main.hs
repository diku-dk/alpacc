module Main where

import qualified Data.Text.IO as TextIO
import System.IO
import Alpacc.Generator.Futhark.Generator
import Prelude hiding (last)
import Data.Maybe
import Options.Applicative
import System.FilePath.Posix (stripExtension, takeFileName)
import System.Exit (exitFailure)
import Debug.Trace (traceShow)
import Alpacc.CFG

debug :: Show b => b -> b
debug x = traceShow x x

data Input
  = FileInput FilePath
  | StdInput

data Parameters = Parameters
  { input     :: Input
  , output    :: Maybe String
  , lookback  :: Int
  , lookahead :: Int}

lookbackParameter :: Parser Int
lookbackParameter =
    option auto
      ( long "lookback"
    <> short 'q'
    <> help "The amount of characters used for lookback."
    <> showDefault
    <> value 1
    <> metavar "INT" )

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
stdInput = flag' StdInput
      ( long "stdin"
    <> short 's'
    <> help "Read from stdin.")

inputParameter :: Parser Input
inputParameter = fileInput <|> stdInput

parameters :: Parser Parameters
parameters = Parameters
  <$> inputParameter
  <*> outputParameter
  <*> lookbackParameter
  <*> lookaheadParameter

opts :: ParserInfo Parameters
opts = info (parameters <**> helper)
  ( fullDesc
  <> progDesc "Creates a parallel parser in Futhark using FILE."
  <> header "Alpacc" )


writeFutharkProgram :: String -> String -> IO ()
writeFutharkProgram program_path program = do
  writeFile program_path program
  putStrLn ("The parser " ++ program_path ++ " was created.")

isFileInput :: Input -> Bool
isFileInput StdInput = False
isFileInput (FileInput _) = True

main :: IO ()
main = do
  options <- execParser opts
  let input_method = input options
  let q = lookback options
  let k = lookahead options
  let outfile = output options
  let program_path = case outfile of
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
  
  case generate q k cfg of
    Left e -> do hPutStrLn stderr e
                 exitFailure
    Right program -> writeFutharkProgram program_path program
