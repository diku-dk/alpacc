module Main where

import ParallelParser.Grammar
import ParallelParser.Generator
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

example :: [String]
example = L.singleton <$> "a+[a+a]"

main :: IO ()
main = do
  contents <- getContents
  let grammar = unpackNTTGrammar (read contents :: Grammar NT T)
  let q = 1
  let k = 1
  putStrLn $ futharkKeyGeneration q k grammar