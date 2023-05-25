module Main where

import ParallelParser.Grammar
import ParallelParser.Generator
import ParallelParser.LLP ( leftRecursiveNonterminals )
import Prelude hiding (last)
import Data.Maybe
import Options.Applicative
import Data.Semigroup ((<>))
import Data.String.Interpolate (i)
import System.FilePath.Posix (stripExtension, takeFileName)
import qualified Data.List as List
import Control.Monad
import Control.Exception
import System.Exit (exitFailure)
import Data.Foldable
import ParallelParser.LL (closureAlgorithm, leftFactorNonterminals)
import qualified Data.Set as Set
import Debug.Trace (traceShow)

debug x = traceShow x x

data Input
  = FileInput FilePath
  | StdInput

data Parametars = Parametars
  { input     :: Input
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

fileInput :: Parser Input
fileInput = FileInput <$> argument str (metavar "FILE")

stdInput :: Parser Input
stdInput = flag' StdInput
      ( long "stdin"
    <> short 's'
    <> help "Read from stdin.")

inputs :: Parser Input
inputs = fileInput <|> stdInput

fileParametars :: Parser Parametars
fileParametars = Parametars
  <$> inputs
  <*> lookbackParameter
  <*> lookaheadParameter


opts :: ParserInfo Parametars
opts = info (fileParametars <**> helper)
  ( fullDesc
  <> progDesc "Creates a parallel parser in Futhark using FILE."
  <> header "ParallelParser" )


writeFutharkProgram :: String -> String -> IO ()
writeFutharkProgram program_path program = do
  writeFile (program_path ++ ".fut") program
  putStrLn ("The parser " ++ program_path ++ ".fut was created.")

isFileInput :: Input -> Bool
isFileInput StdInput = False
isFileInput (FileInput _) = True

grammarError :: Grammar String String -> Maybe String
grammarError grammar
  | not $ null nt_dups = Just [i|The given grammar contains duplicate nonterminals because of #{nt_dups_str}.|]
  | not $ null t_dups = Just [i|The given grammar contains duplicate terminals because of #{t_dups_str}.|]
  | not $ null p_dups = Just [i|The given grammar contains duplicate productions because of #{p_dups_str}.|]
  | not $ null left_recursive_nonterminals = Just [i|The given grammar contains left recursion due to the following nonterminals #{trouble_makers}.|]
  -- | not $ null reverse_left_recursive_nonterminals = Just [i|The given grammar contains left recursion when productions right-hands sides are reversed due to the following nonterminals #{reverse_trouble_makers}.|]
  | not $ null left_factors = Just [i|The given grammar contains productions that has common left factors due to the following nonterminals #{left_factors_str}.|]
  | not $ null nonproductive = Just [i|The given grammar contains nonproductive productions due to the following nonterminals #{nonproductive_str}.|]
  | otherwise = Nothing
  where
    nts = Set.fromList $ nonterminals grammar
    nonproductive = nts `Set.difference` closureAlgorithm grammar
    nonproductive_str = List.intercalate ", " $ Set.toList nonproductive
    left_recursive_nonterminals = leftRecursiveNonterminals grammar
    trouble_makers = List.intercalate ", " left_recursive_nonterminals
    reverse_left_recursive_nonterminals = leftRecursiveNonterminals (reverseGrammar grammar)
    reverse_trouble_makers = List.intercalate ", " reverse_left_recursive_nonterminals
    left_factors = leftFactorNonterminals grammar
    left_factors_str = List.intercalate ", " left_factors
    (nt_dups, t_dups, p_dups) = grammarDuplicates grammar
    nt_dups_str = List.intercalate ", " nt_dups
    t_dups_str = List.intercalate ", " t_dups
    p_dups_str = List.intercalate ", " $ fmap showProd p_dups
    unwrapSym (Terminal a) = a
    unwrapSym (Nonterminal a) = a
    showProd (Production nt s) = nt ++ " -> " ++ unwords (fmap unwrapSym s)

main :: IO ()
main = do
  options <- execParser opts
  let input_method = input options
  let q = lookback options
  let k = lookahead options
  let program_path = case input_method of
        StdInput -> "parser"
        FileInput path -> fromJust . stripExtension "cg" $ takeFileName path
  contents <- case input_method of
        StdInput -> getContents
        FileInput path -> readFile path
  let grammar = unpackNTTGrammar (read contents :: Grammar NT T)
  let maybe_program = futharkKeyGeneration q k grammar
  case grammarError grammar of
    Just msg -> putStrLn msg *> exitFailure
    Nothing -> case maybe_program of
        Nothing -> putStrLn [i|The given Grammar is not a LLP(#{q}, #{k}).|] *> exitFailure
        Just program -> writeFutharkProgram program_path program