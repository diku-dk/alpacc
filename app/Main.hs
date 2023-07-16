module Main where

import qualified Data.Text.IO as T
import System.IO
import Alpacc.Grammar
import Alpacc.Generator
import Prelude hiding (last)
import Data.Maybe
import Options.Applicative
import Data.String.Interpolate (i)
import System.FilePath.Posix (stripExtension, takeFileName)
import qualified Data.List as List
import System.Exit (exitFailure)
import Alpacc.LL (closureAlgorithm)
import qualified Data.Set as Set
import Alpacc.CFG
import Alpacc.Lexer
import Alpacc.RegularExpression
import Debug.Trace (traceShow)

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

grammarError :: (Ord nt, Ord t, Show nt, Show t) => Grammar nt t -> Maybe String
grammarError grammar
  | not $ null nt_dups = Just [i|The given grammar contains duplicate nonterminals because of #{nt_dups_str}.|]
  | not $ null t_dups = Just [i|The given grammar contains duplicate terminals because of #{t_dups_str}.|]
  | not $ null p_dups = Just [i|The given grammar contains duplicate productions because of #{p_dups_str}.|]
  -- | isLeftRecursive grammar = Just [i|The given grammar contains left recursion.|]
  | not $ null nonproductive = Just [i|The given grammar contains nonproductive productions due to the following nonterminals #{nonproductive_str}.|]
  | otherwise = Nothing
  where
    nts = Set.fromList $ nonterminals grammar
    nonproductive = nts `Set.difference` closureAlgorithm grammar
    nonproductive_str = List.intercalate ", " . fmap show $ Set.toList nonproductive
    (nt_dups, t_dups, p_dups) = grammarDuplicates grammar
    nt_dups_str = List.intercalate ", " . fmap show $ nt_dups
    t_dups_str = List.intercalate ", " . fmap show $ t_dups
    p_dups_str = List.intercalate ", " $ fmap show p_dups

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
        StdInput -> T.getContents
        FileInput path -> T.readFile path
  cfg <-
    case cfgFromText program_path contents of
        Left e -> do hPutStrLn stderr e
                     exitFailure
        Right g -> pure g
  grammar <-
      case cfgToGrammar cfg of
        Left e -> do hPutStrLn stderr e
                     exitFailure
        Right g -> pure g
  lexer <-
      case cfgToLexer cfg of
        Left e -> do hPutStrLn stderr e
                     exitFailure
        Right g -> pure g
  regex <-
      case regExFromText "" "(a|b)*" of
        Left e -> do hPutStrLn stderr e
                     exitFailure
        Right g -> pure g
  print regex
  let dfa = dfaFromRegEx 0 regex :: DFA Integer
  print dfa
  print $ isMatch dfa "ababbcbbab"
  let maybe_program = futharkKeyGeneration q k grammar
  case grammarError grammar of
    Just msg -> putStrLn msg *> exitFailure
    Nothing -> case maybe_program of
        Left e -> do hPutStrLn stderr e
                     exitFailure
        Right program ->
          writeFutharkProgram program_path $
            program <>
            generateLexer lexer <>
            "entry parse s = parser.parse (lex char_code accept s)\n"
