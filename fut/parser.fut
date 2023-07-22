-- Start of parser.fut
--
-- The generic LLP parsing machine, expressed as a parameterised
-- module.

import "lib/github.com/diku-dk/sorts/radix_sort"

type bracket = #left u64 | #right u64 | #epsilon
type maybe 'a = #just a | #nothing
type terminal = u32

module type grammar = {
  val q : i64
  val k : i64
  val max_ao : i64
  val max_pi : i64
  type lookahead_type
  type lookback_type
  type transitions_type
  type char
  val lookback_array_to_tuple [n] : [n]terminal -> lookback_type
  val lookahead_array_to_tuple [n] : [n]terminal -> lookahead_type
  val start_terminal : terminal
  val end_terminal : terminal
  val ne : ([max_ao]bracket, [max_pi]terminal)
  val key_to_config : (lookback_type, lookahead_type) -> maybe ([max_ao]bracket, [max_pi]terminal)
  val char_to_transitions : char -> transitions_type
}

module mk_parser(G: grammar) = {
  def lookback_chunks [n] (arr : [n]terminal) =
    let arr' = replicate G.q u32.highest ++ arr
    in iota n |> map (\i -> arr'[i:i + G.q] |> G.lookback_array_to_tuple)

  def lookahead_chunks [n] (arr : [n]terminal) =
    let arr' = arr ++ replicate G.k u32.highest
    in iota n |> map (\i -> arr'[i:i + G.k] |> G.lookahead_array_to_tuple)

  def keys [n] (arr : [n]terminal) =
    let lookback = lookback_chunks arr
    let lookahead = lookahead_chunks arr
    in zip lookback lookahead

  def is_left (b : bracket) : bool =
    match b
    case #left _ -> true
    case #right _ -> false
    case #epsilon -> false

  def depths [n] (input : [n]bracket) : maybe ([n]i64) =
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
       then #nothing
       else #just result

  def grade (xs : []i64) =
    zip xs (indices xs)
    |> radix_sort_int_by_key (.0) i64.num_bits i64.get_bit
    |> map (.1)

  def even_indices 'a [n] (_ : [n]a) =
    iota (n / 2) |> map (2*)

  def unpack_bracket (b : bracket) : u64 =
    match b
    case #left a -> a
    case #right a -> a
    case #epsilon -> assert false 0

  def eq_no_bracket (a : bracket) (b : bracket) : bool =
    unpack_bracket a u64.== unpack_bracket b

  def brackets_matches [n] (brackets : [n]bracket) =
    match depths brackets
    case #just depths' ->
      let grade' = grade depths'
      in even_indices grade'
         |> map (\i -> eq_no_bracket brackets[grade'[i]] brackets[grade'[i+1]])
         |> and
    case #nothing -> false

  def is_nothing 'a (m : maybe a) : bool =
    match m
    case #just _ -> false
    case #nothing -> true

  def from_just 'a (ne : a) (m : maybe a) : a =
    match m
    case #just a -> a
    case _ -> ne

  def parse [n] (arr : [n]u32) : []u32 =
    let arr' = replicate 1 G.start_terminal ++ (map (+2) arr) ++ replicate 1 G.end_terminal
    let configs = keys arr' |> map G.key_to_config
    in if any (is_nothing) configs
       then []
       else
       let (brackets, productions) = unzip (map (from_just G.ne) configs)
       in if brackets |> flatten |> filter (!=#epsilon) |> brackets_matches
          then productions
               |> flatten
               |> filter (!=u32.highest)
               |> tail
               |> map (\a -> a - 1)
          else []
}

-- End of parser.fut
