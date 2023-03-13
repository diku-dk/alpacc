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

example :: [String]
example = L.singleton <$> "a+[a+a]"

main :: IO ()
main = do
  contents <- getContents
  let grammar = unpackNTTGrammar (read contents :: Grammar NT T)
  let augmented_grammar = augmentGrammar grammar
  let collection = llpCollection 2 2 augmented_grammar
  let llp_parsing_table = llpParsingTable 2 2 grammar
  mapM_ printLlpItems collection
  mapM_ print . M.toList . M.filter (1 `elem`) $ (\(_, _, c) -> c) <$> llp_parsing_table
  print $ llpParse 2 2 grammar example
