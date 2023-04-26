{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
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
import ParallelParser.LL
import Prelude hiding (last)
import Data.Bifunctor (Bifunctor (bimap))
import ParallelParser.LL
import Control.Parallel.Strategies
debug x = traceShow x x

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

auxiliary llTableParse' (x, y) alpha = f <$> llTableParse' y alpha
  where
    f (epsilon, omega, pi) = pi

followExtendedGrammar =
  Grammar
    { start = "T'",
      terminals = ["$", "a", "b", "c"],
      nonterminals = ["T'", "R", "T"],
      productions =
        [ Production "T'" [Nonterminal "T", Terminal "$"],
          Production "T" [Nonterminal "R"],
          Production "T" [Terminal "a", Nonterminal "T", Terminal "c"],
          Production "R" [],
          Production "R" [Nonterminal "R", Terminal "b", Nonterminal "R"]
        ],
      leftPadding = Nothing,
      rightPadding = Just "$"
    }

main :: IO ()
main = do
  options <- execParser opts
  let grammar_path = path options
  let q = lookback options
  let k = lookahead options
  let Just program_path = stripExtension "cg" $ takeFileName grammar_path
  contents <- readFile grammar_path
  let grammar = unpackNTTGrammar (read contents :: Grammar NT T)
  let left_recursive_nonterminals = leftRecursiveNonterminals grammar
  let trouble_makers = List.intercalate ", " left_recursive_nonterminals
  let augmented_grammar = augmentGrammar q k grammar
  let Just table = llpParsingTable q k augmented_grammar
  let collection = llpCollection q k augmented_grammar
  let psls_table = psls collection
  let unwrapped = (\[a] -> a) . S.toList <$> psls_table
  let llTableParse' = llTableParse k augmented_grammar
  let ll_table = llTable k augmented_grammar
  let nt = "T"
  let aug_nt = AugmentedNonterminal "T"
  let first' = first k grammar [Nonterminal nt]
  let extended_grammar = extendGrammar k grammar
  let follow' = follow k followExtendedGrammar
  let naiveFollow' = naiveFollow k followExtendedGrammar
  let aug_first' = first k augmented_grammar [Nonterminal aug_nt]
  let aug_follow' = follow k augmented_grammar aug_nt
  putStrLn "LLP Table"
  mapM_ print $ M.toList table
  -- mapM_ (\x -> do putStrLn " "; mapM_ print x) collection
  -- putStrLn "Missing parses"
  -- mapM_ print . M.toList . M.filterWithKey ((isNothing . ).  auxiliary llTableParse') $ unwrapped
  -- putStrLn "LL Table"
  -- mapM_ print . M.toList $ ll_table
  -- putStrLn $ "first(" ++ nt ++ ")"
  -- let strings = symbols <$> productions grammar
  -- putStrLn "Correct Follow sets"
  -- mapM_ print $ (\a -> (a, naiveFollow' a)) <$> nonterminals followExtendedGrammar
  -- putStrLn "Computed Follow sets"
  -- mapM_ print $ (\a -> (a, follow' a)) <$> nonterminals followExtendedGrammar