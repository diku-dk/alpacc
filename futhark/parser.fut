-- Start of parser.fut
--
-- The generic LLP parsing machine, expressed as a parameterised
-- module.

import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/containers/opt"

module type parser_context = {
  module terminal_module : integral
  module production_module : integral
  module bracket_module : integral
  val empty_terminal : terminal_module.t
  val empty_production : production_module.t
  val epsilon : bracket_module.t
  val q : i64
  val k : i64
  val max_ao : i64
  val max_pi : i64
  type lookahead_type
  type lookback_type
  val number_of_terminals : i64
  val lookback_array_to_tuple [n] : [n]terminal_module.t -> lookback_type
  val lookahead_array_to_tuple [n] : [n]terminal_module.t -> lookahead_type
  val start_terminal : terminal_module.t
  val end_terminal : terminal_module.t
  val ne : ([max_ao]bracket_module.t, [max_pi]production_module.t)
  val key_to_config : (lookback_type, lookahead_type) -> opt ([max_ao]bracket_module.t, [max_pi]production_module.t)
}

module mk_parser(P: parser_context) = {
  module terminal_module = P.terminal_module
  module production_module = P.production_module
  module bracket_module = P.bracket_module

  type terminal = terminal_module.t
  type production = production_module.t
  type bracket = bracket_module.t
  
  def empty_terminal = P.empty_terminal
  def empty_production = P.empty_production
  def epsilon = P.epsilon

  def is_left (s : bracket) : bool =
    bracket_module.get_bit (bracket_module.num_bits - 1) s
    |> bool.i32
  
  def lookback_chunks [n] (arr : [n]terminal) : [n]P.lookback_type =
    let arr' = replicate P.q empty_terminal ++ arr
    in iota n |> map (\i -> arr'[i:i + P.q] |> P.lookback_array_to_tuple)

  def lookahead_chunks [n] (arr : [n]terminal) : [n]P.lookahead_type =
    let arr' = arr ++ replicate P.k empty_terminal
    in iota n |> map (\i -> arr'[i:i + P.k] |> P.lookahead_array_to_tuple)

  def keys [n] (arr : [n]terminal) : [n](P.lookback_type, P.lookahead_type) =
    let lookback = lookback_chunks arr
    let lookahead = lookahead_chunks arr
    in zip lookback lookahead

  def depths [n] (input : [n]bracket) : opt ([n]i64) =
    let left_brackets =
      input
      |> map (is_left)
    let bracket_scan =
      left_brackets
      |> map (\b -> if b then 1 else -1)
      |> scan (+) 0
    let result =
      bracket_scan
      |> map2 (\a b -> b - i64.bool a) left_brackets
    in if any (<0) bracket_scan || last bracket_scan != 0
       then #none
       else #some result

  def grade [n] (xs : [n]i64) : [n]i64 =
    zip xs (indices xs)
    |> radix_sort_int_by_key (.0) i64.num_bits i64.get_bit
    |> map (.1)

  def even_indices 'a [n] (_ : [n]a) : [n / 2]i64 =
    iota (n / 2) |> map (2*)

  def unpack_bracket (b : bracket) : bracket =
    bracket_module.set_bit (bracket_module.num_bits - 1) b 0

  def eq_no_bracket (a : bracket) (b : bracket) : bool =
    unpack_bracket a bracket_module.== unpack_bracket b

  def brackets_matches [n] (brackets : [n]bracket) : bool =
    match depths brackets
    case #some depths' ->
      let grade' = grade depths'
      in even_indices grade'
         |> map (\i -> eq_no_bracket brackets[grade'[i]] brackets[grade'[i+1]])
         |> and
    case #none -> false

  def parse [n] (arr : [n]terminal) : []production =
    let arr' = [P.start_terminal] ++ arr ++ [P.end_terminal]
    let configs = keys arr' |> map P.key_to_config
    in if any (is_none) configs
       then []
       else
       let (brackets, productions) = unzip (map (from_opt P.ne) configs)
       in if brackets |> flatten |> filter (bracket_module.!=epsilon) |> brackets_matches
          then productions
               |> flatten
               |> filter (production_module.!=empty_production)
               |> tail
               |> map (\a -> a production_module.- (production_module.i64 1))
          else []

}

-- End of parser.fut
