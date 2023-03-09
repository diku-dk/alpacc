module Main where


import ParallelParser.Grammar
import ParallelParser.Parser
import qualified Data.Map as M
import Prelude hiding (last)
import Text.ParserCombinators.ReadP
import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Set as S
import Data.Sequence
import Data.Maybe
import Debug.Trace (traceShow)
import Data.Foldable

debug x = traceShow ("DEBUG: " ++ show x) x

printLlpItems set = do
  mapM_ print set
  putStrLn ""

main :: IO ()
main = do
  contents <- getContents
  let grammar = read contents :: Grammar NT T
  let collection = llpCollection 1 1 grammar
  mapM_ printLlpItems collection
  mapM_ print . M.toList $ psls collection