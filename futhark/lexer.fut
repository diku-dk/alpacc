-- Start of lexer.fut
--
-- The generic plexer, expressed as a parameterised
-- module.

import "lib/github.com/diku-dk/containers/bitset"
import "lib/github.com/diku-dk/containers/maybe"

module bitset_u8 = mk_bitset u8

module type lexer_context = {
  val transitions_size : i64
  val accepting_size : i64
  val number_of_states : i64
  val number_of_terminals : i64
  val initial_state : i64
  val accepting_states : [accepting_size]i64
  val dead_transitions : [transitions_size](i64, i64)
  val final_terminal_states : [number_of_terminals](bitset_u8.bitset[(number_of_states - 1) / bitset_u8.nbs + 1])
  val continue_terminal_states : [number_of_terminals](bitset_u8.bitset[(number_of_states - 1) / bitset_u8.nbs + 1])
  val char_to_transitions : u8 -> maybe ([transitions_size](i64, i64))
  val transition_to_terminal_set : ((i64, i64), u8) -> bitset_u8.bitset[(number_of_terminals - 1) / bitset_u8.nbs + 1]
  val is_ignore : i64 -> bool
}

module mk_lexer(L: lexer_context) = {
  type bitset = bitset_u8.bitset[(L.number_of_terminals - 1) / bitset_u8.nbs + 1] 
  type transition_type = (i64, i64)
  type trans_vec = [L.transitions_size]transition_type
  
  def combine (a : transition_type) (b : transition_type) : transition_type =
    (a.0, b.1)

  def combine_transitions [n] (a : maybe ([n]transition_type)) (b : maybe ([n]transition_type)) : maybe ([n]transition_type) =
    add_identity (map2 combine) a b

  def solve_transitions [n] (arr : [n](maybe trans_vec)) : [n]trans_vec =
    scan combine_transitions #nothing arr
    |> map (from_maybe (copy L.dead_transitions))

  def transitions [n] (str : [n]u8) : [n](maybe ([]transition_type)) =
    map L.char_to_transitions str

  def minimum (set : bitset) : i64 =
    let m = bitset_u8.to_array set
        |> reduce_comm (i64.min) L.number_of_terminals
    in if m == L.number_of_terminals
       then -1
       else m
  
  def solve_overlap (set : bitset) (set' : bitset) : bitset =
    let set'' = set `bitset_u8.intersection` set'
    in if bitset_u8.size set'' == 0
       then set'
       else set''

  def empty = bitset_u8.empty L.number_of_terminals

  def full_set = bitset_u8.complement empty

  def solve_overlaps [n] (sets : [n]bitset) =
    reverse sets
    |> scan solve_overlap full_set
    |> map minimum
    |> reverse
  
  def combine_trans_vec [n] (a : [n](i64, i64)) (b : [n](i64, i64)) : [n](i64, i64) =
    map2 (\a' b' ->
      if a'.0 < 0 || a'.1 < 0
      then b'
      else if b'.0 < 0 || b'.1 < 0
           then a'
           else b[a'.1]
    ) a b
  
  def trans_vec_identity : trans_vec =
    replicate L.transitions_size (-1, -1)

  def find_path [n] (trans: [n]trans_vec) : [n]transition_type =
    [replicate L.transitions_size (L.initial_state, L.initial_state)]
    |> (++trans)
    |> scan combine_trans_vec (copy trans_vec_identity)
    |> tail
    |> map (.[0])
    |> sized n

  def is_terminal [n] (terminal_strings : [n]i64) (path : [n]transition_type) (i : i64) : bool =
    not (L.is_ignore terminal_strings[i]) &&
    (i == 0 ||
     terminal_strings[i] != terminal_strings[i - 1] ||
     (bitset_u8.member path[i - 1].1 L.final_terminal_states[terminal_strings[i]] &&
      not (bitset_u8.member path[i - 1].1 L.continue_terminal_states[terminal_strings[i]])))

  def lexer [n] (str : [n]u8) : maybe ([]u32, [](i64, i64)) =
    if n == 0
    then #just ([], [])
    else let path =
           transitions str
           |> solve_transitions
           |> find_path
         in if any (==path[n - 1].1) L.accepting_states |> not
            then #nothing
            else let terminal_strings =
                  zip path str
                  |> map L.transition_to_terminal_set
                  |> solve_overlaps
                 in let terminal_starts = filter (is_terminal terminal_strings path) (indices terminal_strings)
                    let terminals = map (\i -> u32.i64 terminal_strings[i]) terminal_starts
                    let len = length terminal_starts - 1
                    let spans =
                      map (\i ->
                        if i == len
                        then (terminal_starts[i], n - 1)
                        else (terminal_starts[i], terminal_starts[i + 1] - 1)
                      ) (indices terminal_starts)
                    in #just (terminals, spans)

}

-- End of lexer.fut
