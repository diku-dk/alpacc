-- Start of parser.fut
--
-- The generic LLP parsing machine, expressed as a parameterised
-- module.

import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/containers/bitset"
import "lib/github.com/diku-dk/containers/maybe"

type bracket = #left u64 | #right u64 | #epsilon
type terminal = u32

module bitset_u8 = mk_bitset u8

module type grammar = {
  val q : i64
  val k : i64
  val max_ao : i64
  val max_pi : i64
  val transitions_size : i64
  val accepting_size : i64
  type lookahead_type
  type lookback_type
  type char
  val number_of_terminals : i64
  val initial_state : i64
  val dead_transition : (i64, i64)
  val accepting_states : [accepting_size]i64
  val dead_transitions : [transitions_size](i64, i64)
  val lookback_array_to_tuple [n] : [n]terminal -> lookback_type
  val lookahead_array_to_tuple [n] : [n]terminal -> lookahead_type
  val start_terminal : terminal
  val end_terminal : terminal
  val ne : ([max_ao]bracket, [max_pi]terminal)
  val key_to_config : (lookback_type, lookahead_type) -> maybe ([max_ao]bracket, [max_pi]terminal)
  val char_to_transitions : char -> maybe ([transitions_size](i64, i64))
  val transition_to_terminal_set : ((i64, i64), char) -> bitset_u8.bitset[(number_of_terminals - 1) / bitset_u8.nbs + 1]
}

module mk_parser(G: grammar) = {
  type bitset = bitset_u8.bitset[(G.number_of_terminals - 1) / bitset_u8.nbs + 1] 
  type transition_type = (i64, i64)

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
    case _ -> false

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

  def parse [n] (arr : [n]u32) : []u32 =
    let arr' = [G.start_terminal] ++ arr ++ [G.end_terminal]
    let configs = keys arr' |> map G.key_to_config
    in if any (is_nothing) configs
       then []
       else
       let (brackets, productions) = unzip (map (from_maybe G.ne) configs)
       in if brackets |> flatten |> filter (!=#epsilon) |> brackets_matches
          then productions
               |> flatten
               |> filter (!=u32.highest)
               |> tail
               |> map (\a -> a - 1)
          else []
  
  def combine (a : transition_type) (b : transition_type) : transition_type =
    (a.0, b.1)

  def combine_transitions [n] (a : maybe ([n]transition_type)) (b : maybe ([n]transition_type)) : maybe ([n]transition_type) =
    add_identity (map2 combine) a b

  def solve_transitions [n] (arr : [n](maybe ([G.transitions_size]transition_type))) : [n][G.transitions_size]transition_type =
    scan combine_transitions #nothing arr
    |> map (from_maybe (copy G.dead_transitions))

  def transitions [n] (str : [n]G.char) : [n](maybe ([]transition_type)) =
    map G.char_to_transitions str
  
  -- | This is completely wrong it should be done in parallel.
  def find_path [n] (trans: [n][G.transitions_size]transition_type) : [n]transition_type =
    (loop result = (replicate n (-1, -1), G.initial_state) for i < n do
      let old_arr = result.0
      let old_st = result.1
      let new_arr = old_arr with [i] = trans[i][old_st]
      let new_st = new_arr[i].1
      in (new_arr, new_st)
    ) |> (.0)

  def minimum (set : bitset) : i64 =
    let m = bitset_u8.to_array set
        |> reduce_comm (i64.min) G.number_of_terminals
    in if m == G.number_of_terminals
       then -1
       else m
  
  def solve_overlap (set : bitset) (set' : bitset) : bitset =
    let set'' = set `bitset_u8.intersection` set'
    in if bitset_u8.size set'' == 0
       then set'
       else set''

  def empty = bitset_u8.empty G.number_of_terminals

  def full_set = bitset_u8.complement empty

  def solve_overlaps [n] (sets : [n]bitset) =
    reverse sets
    |> scan solve_overlap full_set
    |> reverse

  def lexer [n] (str : [n]G.char) =
    let path =
      transitions str
      |> solve_transitions
      |> find_path
    in if not <| any (==path[n - 1].1) G.accepting_states
       then []
       else zip path str
            |> map G.transition_to_terminal_set
            |> solve_overlaps
  
}

-- End of parser.fut
