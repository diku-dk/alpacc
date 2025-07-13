module Alpacc.Generator.Futhark.Parser
  ( generateParser,
  )
where

import Alpacc.Generator.Futhark.Futharkify
import Alpacc.Generator.Generator
import Alpacc.Generator.Util
import Alpacc.Grammar
import Alpacc.HashTable
import Alpacc.LLP
  ( Bracket (..),
    llpParserTableWithStartsHomomorphisms,
  )
import Alpacc.Types
import Control.DeepSeq
import Data.Bifunctor qualified as BI
import Data.Composition
import Data.FileEmbed
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tuple.Extra

futharkParser :: Text
futharkParser = $(embedStringFile "futhark/parser.fut")

futharkifyBracket :: (Futharkify a) => Bracket a -> RawString
futharkifyBracket (LBracket a) = RawString . ("left " <>) $ futharkify a
futharkifyBracket (RBracket a) = RawString . ("right " <>) $ futharkify a

-- | Creates Futhark source code which contains a parallel parser that can
-- create the productions list for a input which is indexes of terminals.
generateParser :: UInt -> Parser -> Text
generateParser terminal_type parser =
  futharkParser
    <> (Text.strip . Text.pack)
      [i|
module parser = mk_parser {

module terminal_module = #{futharkify terminal_type}
module production_module = #{futharkify production_type}
module bracket_module = #{futharkify bracket_type}

type terminal = terminal_module.t
type production = production_module.t
type bracket = bracket_module.t

def empty_terminal : terminal = terminal_module.highest
def empty_production : production = production_module.highest
def epsilon : bracket = bracket_module.highest

def left (s : bracket) : bracket =
  bracket_module.set_bit (bracket_module.num_bits - 1) s 1

def right (s : bracket) : bracket =
  bracket_module.set_bit (bracket_module.num_bits - 1) s 0

def number_of_terminals: i64 = #{number_of_terminals}
def number_of_productions: i64 = #{number_of_productions} 
def hash_table_level_one_size: i64 = #{hashTableLevelOneSize hash_table}
def hash_table_level_two_size: i64 = #{hashTableLevelTwoSize hash_table}
def q: i64 = #{q}
def k: i64 = #{k}
def start_terminal: terminal = #{start_terminal}
def end_terminal: terminal = #{end_terminal}
def production_to_terminal: [number_of_productions](opt terminal) =
  #{prods_to_ters}
def production_to_arity: [number_of_productions]i16 = sized number_of_productions 

def level_two_offsets: [hash_table_level_two_size]i64 =
  #{futharkify $ levelTwoOffsets hash_table} :> [hash_table_level_two_size]i64

def level_two_shape: [hash_table_level_two_size]i64 =
  #{futharkify $ sizeArray hash_table} :> [hash_table_level_two_size]i64

def level_one_keys_offsets: [hash_table_level_one_size]i64 =
  #{futharkify $ levelOneKeysOffsets hash_table} :> [hash_table_level_one_size]i64

def level_one_stacks_offsets: [hash_table_level_two_size]i64 =
  #{futharkify $ levelOneStacksOffsets hash_table} :> [hash_table_level_two_size]i64

def level_one_productions_offsets: [hash_table_level_two_size]i64 =
  #{futharkify $ levelOneProductionsOffsets hash_table} :> [hash_table_level_two_size]i64

def keys_array: [hash_table_level_two_size][q + k]terminal =
  #{futharkify $ keysArray hash_table} :> [hash_table_level_two_size][q + k]terminal

def stacks_size: i64 =
  #{futharkify $ stacksSize hash_table}

def productions_size: i64 =
  #{futharkify $ productionsSize hash_table}

def stacks_array: [stacks_size]bracket  =
  #{futharkify brackets} :> [stacks_size]bracket

def productions_array: [productions_size]production =
  #{futharkify $ productionsArray hash_table} :> [productions_size]production

def stacks_shape: [hash_table_level_two_size]i64 =
  #{futharkify $ levelOneStacksShape hash_table} :> [hash_table_level_two_size]i64

def productions_shape: [hash_table_level_two_size]i64 =
  #{futharkify $ levelOneProductionsShape hash_table} :> [hash_table_level_two_size]i64

def level_one_consts: [hash_table_level_two_size][q + k]i64 =
  #{futharkify $ constsArray hash_table} :> [hash_table_level_two_size][q + k]i64

def level_two_consts: [q + k]i64 =
  #{futharkify $ initHashConsts hash_table} :> [q + k]i64
}
|]
  where
    production_type = productionType parser
    bracket_type = bracketType parser
    q = lookback parser
    k = lookahead parser
    start_terminal = startTerminal parser
    end_terminal = endTerminal parser
