module Alpacc.Generator.Futhark.Parser
  ( generateParser,
  )
where

import Alpacc.Generator.Futhark.Futharkify
import Alpacc.Generator.Generator
import Alpacc.Generator.Util
import Alpacc.HashTable
import Alpacc.LLP
  ( Bracket (..),
  )
import Alpacc.Types
import Data.FileEmbed
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text

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

def left (s : bracket) : bracket =
  bracket_module.set_bit (bracket_module.num_bits - 1) s 1

def right (s : bracket) : bracket =
  bracket_module.set_bit (bracket_module.num_bits - 1) s 0

def number_of_productions: i64 = #{number_of_productions} 
def q: i64 = #{q}
def k: i64 = #{k}
def empty_terminal: terminal = #{empty_terminal}
def start_terminal: terminal = #{start_terminal}
def end_terminal: terminal = #{end_terminal}
def production_to_terminal: [number_of_productions](opt terminal) =
  #{production_to_terminal} :> [number_of_productions](opt terminal)
def production_to_arity: [number_of_productions]i64 =
  #{ari} :> [number_of_productions]i64
def hash_table_size: i64 = #{length $ oaArray oa}
def max_iters: i64 = #{oaMaxIters oa}
def productions_size: i64 = #{productions_size}
def stacks_size: i64 = #{stacks_size}

def hash_table: [hash_table_size](bool, [q+k]terminal, ((i64, i64), (i64, i64))) =
  #{futharkify $ oaArray oa} :> [hash_table_size](bool, [q+k]terminal, ((i64, i64), (i64, i64)))

def stacks: [stacks_size]bracket =
  #{futharkify stacks} :> [stacks_size]bracket

def productions: [productions_size]production =
  #{futharkify productions} :> [productions_size]production
}
|]
  where
    production_type = productionType parser
    bracket_type = bracketType parser
    q = lookback parser
    k = lookahead parser
    start_terminal = startTerminal parser
    end_terminal = endTerminal parser
    empty_terminal = emptyTerminal parser
    hash_table = llpTable parser
    number_of_productions = numberOfProductions parser
    stacks = futharkifyBracket <$> llpStacks hash_table
    productions = llpProductions hash_table
    stacks_size = length $ llpStacks hash_table
    productions_size = length $ llpProductions hash_table
    oa = llpOATable hash_table
    production_to_terminal = futharkify $ productionToTerminal parser
    ari = futharkify $ arities parser
