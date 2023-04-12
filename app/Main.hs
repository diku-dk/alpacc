module Main where

import ParallelParser.Grammar
import ParallelParser.Generator
import ParallelParser.LLP
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
import System.FilePath.Posix (stripExtension, takeFileName)
import qualified Data.List as List

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


writeFutharkProgram :: String -> String -> IO ()
writeFutharkProgram program_path program = do
  writeFile (program_path ++ ".fut") program 
  putStrLn ("The parser " ++ program_path ++ ".fut was created.")


main :: IO ()
main = do
  options <- execParser opts
  let grammar_path = path options
  let q = lookback options
  let k = lookahead options
  let Just program_path = stripExtension "cg" $ takeFileName grammar_path 
  contents <- readFile grammar_path
  let grammar = unpackNTTGrammar (read contents :: Grammar NT T)
  let maybe_program = futharkKeyGeneration q k grammar
  let left_recursive_pairs = leftRecursiveNonterminals grammar 
  let trouble_makers = List.intercalate ", " left_recursive_pairs
  print trouble_makers
  if [] /= left_recursive_pairs
  then
    putStrLn [i|The given grammar contains left recursion due to the following nonterminals #{trouble_makers}.|]
  else
    case maybe_program of
      Nothing -> putStrLn [i|The given Grammar may not be LLP(#{q}, #{k}).|]
      Just program -> writeFutharkProgram program_path program