-- Start of lexer.fut
--
-- The generic parallel lexer, expressed as a parameterised
-- module.

import "lib/github.com/diku-dk/containers/bitset"
import "lib/github.com/diku-dk/containers/maybe"

module bitset_u8 = mk_bitset u8

module type lexer_context = {
  module state_module : integral
  module char_module : integral
  module terminal_module : integral
  val transitions_size : i64
  val accepting_size : i64
  val number_of_states : i64
  val number_of_terminals : i64
  val initial_state : state_module.t
  val dead_state : state_module.t
  val accepting_states : [accepting_size]state_module.t
  val dead_transitions : [transitions_size](state_module.t, state_module.t)
  val final_terminal_states : [number_of_terminals](bitset_u8.bitset[(number_of_states - 1) / bitset_u8.nbs + 1])
  val continue_terminal_states : [number_of_terminals](bitset_u8.bitset[(number_of_states - 1) / bitset_u8.nbs + 1])
  val inverted_final_terminal_states : [number_of_states](bitset_u8.bitset[(number_of_terminals - 1) / bitset_u8.nbs + 1])
  val inverted_continue_terminal_states : [number_of_states](bitset_u8.bitset[(number_of_terminals - 1) / bitset_u8.nbs + 1])
  val char_to_transitions : char_module.t -> maybe ([transitions_size](state_module.t, state_module.t))
  val transition_to_terminal_set : ((state_module.t, state_module.t), char_module.t) -> bitset_u8.bitset[(number_of_terminals - 1) / bitset_u8.nbs + 1]
  val is_ignore : terminal_module.t -> bool
}

module mk_lexer(L: lexer_context) = {
  module terminal_module = L.terminal_module
  module state_module = L.state_module
  module char_module = L.char_module

  type terminal = terminal_module.t
  type state = state_module.t
  type char = char_module.t

  type bitset = bitset_u8.bitset[(L.number_of_terminals - 1) / bitset_u8.nbs + 1] 
  type transition = (state, state)
  type transition_vector = [L.transitions_size]transition
  
  def combine (a : transition) (b : transition) : transition =
    (a.0, b.1)

  def combine_transitions [n] (a : maybe ([n]transition)) (b : maybe ([n]transition)) : maybe ([n]transition) =
    add_identity (map2 combine) a b

  def solve_transitions [n] (arr : [n](maybe transition_vector)) : [n](maybe transition_vector) =
    scan combine_transitions #nothing arr

  def transitions [n] (str : [n]char) : [n](maybe ([]transition)) =
    map L.char_to_transitions str

  def minimum (set : bitset) : terminal =
    let m = bitset_u8.to_array set
        |> reduce_comm (i64.min) L.number_of_terminals
    in terminal_module.i64 m
  
  def solve_overlap [n] (path: [n]transition) ((j, set) : (i64, bitset)) ((i, set') : (i64, bitset)) : (i64, bitset) =
    if i == -1 || j == -1 || any (state_module.==path[j].1) L.accepting_states
    then (i, set')
    else (i, set `bitset_u8.intersection` set')

  def empty = bitset_u8.empty L.number_of_terminals

  def full_set = bitset_u8.complement empty

  def solve_overlaps [n] (path : [n]transition) (sets : [n]bitset) =
    zip (iota n) sets
    |> reverse
    |> scan (solve_overlap path) (-1, full_set)
    |> reverse
    |> map (.1)

  def compose_transition_vectors (a : transition_vector) (b : transition_vector) : transition_vector =
    map (\a' ->
      let b_new = b[state_module.to_i64 a'.1]
      in if a'.1 state_module.== L.dead_state || b_new.1 state_module.== L.dead_state
      then b[state_module.to_i64 L.initial_state]
      else b[state_module.to_i64 a'.1]
    ) a
  
  def compose_transition_vectors_identity =
    add_identity compose_transition_vectors

  def find_path [n] (transition_vectors: [n](maybe transition_vector)) : [1 + n]transition =
    [#just (replicate L.transitions_size (L.initial_state, L.initial_state))]
    |> (++transition_vectors)
    |> scan compose_transition_vectors_identity #nothing
    |> map (head <-< from_maybe (copy L.dead_transitions))

  def is_terminal [n] (terminal_strings : [n]terminal) (path : [n]transition) (i : i64) : bool =
    let current_terminal = terminal_strings[i]
    let last_state = state_module.to_i64 path[i64.max (i - 1) 0].1
    let final_bitset = L.final_terminal_states[terminal_module.to_i64 current_terminal]
    let continue_bitset = L.continue_terminal_states[terminal_module.to_i64 current_terminal]
    in not (L.is_ignore current_terminal) &&
       (i == 0 ||
        current_terminal terminal_module.!= terminal_strings[i - 1] ||
        (bitset_u8.member last_state final_bitset &&
         not (bitset_u8.member last_state continue_bitset)
        )
       )
  
  def find_terminal_strings path str =
    zip path str
    |> map L.transition_to_terminal_set
    |> solve_overlaps path

  def lex [n] (str : [n]char) : maybe ([]terminal, [](i64, i64)) =
    let path_with_init =
      transitions str
      |> solve_transitions
      |> find_path
    let path = tail path_with_init |> sized n
    let terminal_strings = map minimum <| find_terminal_strings path str
    let terminal_starts = filter (is_terminal terminal_strings path) (indices terminal_strings)
    let terminals = map (\i -> terminal_strings[i]) terminal_starts
    let len = length terminal_starts - 1
    let spans =
      map (\i ->
        if i == len
        then (terminal_starts[i], n - 1)
        else (terminal_starts[i], terminal_starts[i + 1] - 1)
      ) (indices terminal_starts)
    in if any (state_module.==path_with_init[n].1) L.accepting_states
       then #just (terminals, spans)
       else #nothing
}

-- End of lexer.fut
