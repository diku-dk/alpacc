module Alpacc.Generator.Futhark.Lexer
  ( generateLexer )
where

import Alpacc.Grammar
import Alpacc.Lexer.DFA
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.String.Interpolate (i)
import Data.Tuple.Extra
import Data.FileEmbed
import Alpacc.Generator.Futhark.Util
import Data.Word (Word8)
import Alpacc.Lexer.FSA
import Alpacc.Lexer.ParallelLexing
import Data.List qualified as List

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

terminalTable :: Map ((Int, Int), Int) Int -> String
terminalTable table =
  [i|
def transition_to_terminal (n : ((i64, i64), u8)) : i64 =
  match n
  #{cases}
  case _ -> dead_terminal
|]
  where
    cases = futharkTableCases . Map.toAscList $ _table
    toStr ((x, y), z) = [i|((#{x}, #{y}), #{z})|]
    _table = show <$> Map.mapKeys toStr table

initialLoop :: Set Int -> String
initialLoop set =
  (++"]")
  $ ("["++)
  $ List.intercalate ", "
  $ (\j -> if j `Set.member` set then "true" else "false") <$> [0..255]

acceptingStates :: Set Int -> String
acceptingStates set =
  (++"]")
  $ ("["++)
  $ List.intercalate ", "
  $ (\j -> if j `Set.member` set then "true" else "false") <$> Set.toList set

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

def states_size: i64 = #{Set.size $ states dfa}
def accepting_states: [states_size]bool = #{acceptingStates $ states dfa} |> sized states_size 
def dead_terminal: i64 = #{dead_terminal}
def dead_state: i64 = #{dead_state}
def dead_endomorphism: i64 = #{dead_endomorphism}
def identity_endomorphism: i64 = #{identity_endomorphism}
def initial_state: i64 = #{initial_state}
def initial_loop_set: [256]bool = #{initialLoop initial_loop_set}

#{ignore_function}

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
    (initial_loop_set, state_map, compositions, terminal_map', dead_state, dead_endomorphism, identity_endomorphism) =
      complete lexer
    ignore_function = 
      case T "ignore" `Map.lookup` terminal_index_map of
        Just j -> [i|def is_ignore (t : i64) : bool = #{j} == t|]
        Nothing -> [i|def is_ignore (_ : i64) : bool = false|]
