module Alpacc.Generator.Futhark.Parser
  ( generateParser,
  )
where

import Alpacc.Encode
import Alpacc.Generator.Analyzer
import Alpacc.Generator.Futhark.Futharkify
import Alpacc.HashTable
import Alpacc.Types
import Data.FileEmbed
import Data.List qualified as List
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text

futharkParser :: Text
futharkParser = $(embedStringFile "futhark/parser.fut")

productionNameType :: [Text] -> Text
productionNameType names =
  "type production = " <> Text.intercalate " | " (List.nub names)

-- | Creates Futhark source code which contains a parallel parser that can
-- create the productions list for a input which is indexes of terminals.
generateParser :: UInt -> Parser -> Text
generateParser terminal_type parser =
  futharkParser
    <> (Text.strip . Text.pack)
      [i|
module parser = mk_parser {
  module terminal_int_module = #{futharkify terminal_type}
  module production_int_module = #{futharkify production_type}
  module bracket_module = #{futharkify bracket_type}
  
  type terminal_int = terminal_int_module.t
  type production_int = production_int_module.t
  type bracket = bracket_module.t
  type terminal = terminal
  #{productionNameType production_names}

  def number_of_terminals = number_of_terminals
  def terminal_int_to_name = terminal_int_to_name :> [number_of_terminals]terminal
  def number_of_productions: i64 = #{number_of_productions} 
  def q: i64 = #{q}
  def k: i64 = #{k}
  def empty_terminal: terminal_int = #{empty_terminal}
  def start_terminal: terminal_int = #{start_terminal}
  def end_terminal: terminal_int = #{end_terminal}
  def production_to_terminal: [number_of_productions](opt terminal_int) =
    #{production_to_terminal} :> [number_of_productions](opt terminal_int)
  def production_to_arity: [number_of_productions]i64 =
    #{ari} :> [number_of_productions]i64
  def hash_table_size: i64 = #{length $ oaArray oa}
  def max_iters: i64 = #{oaMaxIters oa}
  def productions_size: i64 = #{productions_size}
  def stacks_size: i64 = #{stacks_size}
  
  def hash_table: [hash_table_size](bool, [q+k]terminal_int, ((i64, i64), (i64, i64))) =
    #{futharkify $ oaArray oa} :> [hash_table_size](bool, [q+k]terminal_int, ((i64, i64), (i64, i64)))
  
  def stacks: [stacks_size]bracket =
    #{futharkify stacks} :> [stacks_size]bracket
  
  def productions: [productions_size]production_int =
    #{futharkify productions} :> [productions_size]production_int
  
  def production_int_to_name: [number_of_productions]production =
    #{futharkify $ map RawString production_names} :> [number_of_productions]production
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
    stacks = llpStacks hash_table
    productions = llpProductions hash_table
    stacks_size = length $ llpStacks hash_table
    productions_size = length $ llpProductions hash_table
    oa = llpOATable hash_table
    production_to_terminal = futharkify $ productionToTerminal parser
    ari = futharkify $ arities parser
    production_names = ("#" <>) <$> productionToName parser
