module Alpacc.Generator.Futhark.Lexer
  ( generateLexer )
where

import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Data.Map (Map)
import Data.Map qualified as Map
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Tuple.Extra
import Data.Foldable
import Data.FileEmbed
import Alpacc.Generator.Futhark.Util
import Data.List.Split (chunksOf)
import Data.Word (Word8)
import Alpacc.Lexer.FSA
import Control.Monad.Identity (Identity(runIdentity))
import Data.Maybe (fromJust)
import Alpacc.Lexer.ParallelLexing

futharkLexer :: String
futharkLexer = $(embedStringFile "futhark/lexer.fut")

stateTable :: IntMap Int -> String
stateTable table =
    [i|
def endomorphism_to_state (a : i64) : i64 =
  match a
  #{str_lexer_table}
  case _ -> dead_state
|]
  where
    str_lexer_table =
      futharkTableCases
        $ both show <$> IntMap.toList table

terminalTable :: IntMap Int -> String
terminalTable table =
    [i|
def endomorphism_to_terminal (a : i64) : i64 =
  match a
  #{str_lexer_table}
  case _ -> dead_terminal
|]
  where
    str_lexer_table =
      futharkTableCases
        $ both show <$> IntMap.toList table

compositionsTable :: Int -> Map (Int, Int) Int -> String
compositionsTable identity_endomorphism table =
  [i|
def compose (a : i64) (b : i64) : i64 =
  match (a, b)
  #{cases}
  case (_, #{identity_endomorphism}) -> a
  case (#{identity_endomorphism}, _) -> b
  case _ -> dead_endomorphism
|]
  where
    cases = futharkTableCases . Map.toAscList $ _table
    toStr (x, y) = [i|(#{x}, #{y})|]
    _table = show <$> Map.mapKeys toStr table

convertWord8 :: Word8 -> Int
convertWord8 = fromIntegral . toInteger

convertInteger :: Integer -> Int
convertInteger = fromIntegral  

generateLexer ::
  DFALexer Word8 Integer T ->
  Map T Integer ->
  FutUInt ->
  Either String String
generateLexer lexer' terminal_index_map' terminal_type = do
  Right $
    futharkLexer
      <> [i|
module lexer = mk_lexer {

def dead_terminal: i64 = #{dead_terminal}
def dead_state: i64 = #{dead_state}
def dead_endomorphism: i64 = #{dead_endomorphism}
def identity_endomorphism: i64 = #{identity_endomorphism}
def initial_state: i64 = #{initial_state}

#{compositionsTable identity_endomorphism compositions}

#{stateTable state_map}

#{terminalTable terminal_map}
}
|]
  where
    lexer = fsaLexerMap convertWord8 convertInteger lexer'
    dfa = fsa lexer
    initial_state = initial dfa
    terminal_index_map = (\a -> fromIntegral a :: Int) <$> terminal_index_map'
    dead_terminal = succ $ maximum terminal_index_map
    terminal_map = (terminal_index_map Map.!) <$> terminal_map'
    (state_map, compositions, terminal_map', dead_state, dead_endomorphism, identity_endomorphism) =
      complete lexer
    ignore_function = 
      case T "ignore" `Map.lookup` terminal_index_map of
        Just j -> [i|def is_ignore (t : terminal) : bool = #{j} == t|]
        Nothing -> [i|def is_ignore (_ : terminal) : bool = false|]
