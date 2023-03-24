module Main where

import ParallelParser.Grammar
import ParallelParser.Generator
import qualified Data.Map as M
import Prelude hiding (last)
import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Set as S
import Data.Sequence
import Data.Maybe
import Debug.Trace (traceShow)
import Data.Foldable
import Options.Applicative
import Data.Semigroup ((<>))
import Data.String.Interpolate (i)
import System.FilePath.Posix (stripExtension)

data Parametars = Parametars
  { path      :: String
  , lookback  :: Int
  , lookahead :: Int }


parametars :: Parser Parametars
parametars = Parametars
  <$> argument str (metavar "FILE")
  <*> option auto
      ( long "lookback"
    <> short 'q'
    <> help "The amount of characters used for lookback."
    <> showDefault
    <> value 1
    <> metavar "INT" )
  <*> option auto
      ( long "lookahead"
    <> short 'k'
    <> help "The amount of characters used for lookahead."
    <> showDefault
    <> value 1
    <> metavar "INT" )


opts :: ParserInfo Parametars
opts = info (parametars <**> helper)
  ( fullDesc
  <> progDesc "Creates a parallel parser in Futhark using FILE."
  <> header "ParallelParser" )


writeFutharkProgram program_path program = do
  writeFile (program_path ++ ".fut") program 
  putStrLn ("The parser " ++ program_path ++ ".fut was created.")


main :: IO ()
main = do
  options <- execParser opts
  let grammar_path = path options
  let q = lookback options
  let k = lookahead options
  let Just program_path = stripExtension "cfg" grammar_path 
  contents <- readFile grammar_path
  let grammar = unpackNTTGrammar (read contents :: Grammar NT T)
  let maybe_program = futharkKeyGeneration q k grammar
  case maybe_program of
    Nothing -> putStrLn [i|The given Grammar may not be LLP(#{q}, #{k})|]
    Just program -> writeFutharkProgram program_path program