module Main where


import Parser.Grammar
import Parser.Parsing
import qualified Data.Map as M
import Prelude hiding (last)
import Text.ParserCombinators.ReadP
import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Set as S
import Data.Sequence
import Data.Maybe
import Debug.Trace (traceShow)

debug x = traceShow ("DEBUG: " ++ show x) x

example = fromList [Terminal $ T "a", Terminal $ T "+", Terminal $ T "[", Terminal $ T "a", Terminal $ T "+", Terminal $ T "a", Terminal $ T "]"]

printLlpItems :: Show a => S.Set a -> IO ()
printLlpItems set = do
  mapM_ print set
  putStrLn "\n\n"

main :: IO ()
main = do
  contents <- getContents
  let grammar = read contents :: Grammar NT T
  let nullable' = nullable grammar
  let augmented_grammar = augmentGrammar grammar
  let test_production = findProductions augmented_grammar Start
  -- mapM_ print $ (\(Production nt s) -> (nt, s, nullable' s)) <$> productions grammar
  let first' = first 5 grammar
  -- mapM_ print $ (\(Production nt s) -> (nt, s, first' s)) <$> productions grammar
  -- print . zip (productions grammar) $ last 2 grammar . symbols <$> productions grammar
  -- print $ (\(Production nt s) -> (nt, last' s)) <$> productions augmentedGrammar
  -- print $ solveShortestsPrefix grammar (fromList [Terminal $ T "a", Terminal $ T "+", Nonterminal $ NT "T", Nonterminal $ NT "E'"]) example
  -- let (DotProduction nt s s') = (L.!! 2) . moveDots . toDotProduction $ head test_production
  -- mapM_ print $ llpItems 1 1 grammar
  -- let temp = T . L.singleton <$> "aabbbcc$"
  print . S.size $ llpItems 1 1 grammar -- llkParse 1 grammar (temp, [Nonterminal $ start grammar], []) 