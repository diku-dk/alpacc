-- Start of lexer.fut
--
-- The generic parallel lexer, expressed as a parameterised
-- module.

import "lib/github.com/diku-dk/containers/bitset"
import "lib/github.com/diku-dk/containers/maybe"

module bitset_u32 = mk_bitset u32

module type lexer_context = {
  module state_module : integral
  module char_module : integral
  module terminal_module : integral
  val accepting_size : i64
  val number_of_states : i64
  val number_of_terminals : i64
  val initial_state : state_module.t
  val dead_state : state_module.t
  val accepting_states : [accepting_size]state_module.t
  val dead_transitions : [number_of_states](state_module.t, state_module.t)
  val final_terminal_states : [number_of_terminals](bitset_u32.bitset[(number_of_states - 1) / bitset_u32.nbs + 1])
  val inverted_final_terminal_states : [number_of_states](bitset_u32.bitset[(number_of_terminals - 1) / bitset_u32.nbs + 1])
  val char_to_transitions : char_module.t -> [number_of_states]state_module.t
  val transition_to_terminal_set : ((state_module.t, state_module.t), char_module.t) -> bitset_u32.bitset[(number_of_terminals - 1) / bitset_u32.nbs + 1]
  val is_ignore : terminal_module.t -> bool
}

module mk_lexer(L: lexer_context) = {
  module terminal_module = L.terminal_module
  module state_module = L.state_module
  module char_module = L.char_module

  type terminal = terminal_module.t
  type state = state_module.t
  type char = char_module.t

  type terminal_set = bitset_u32.bitset[(L.number_of_terminals - 1) / bitset_u32.nbs + 1]
  type state_vector = [L.number_of_states]state
  type maybe_state_vector = [L.number_of_states](maybe state)

  type transition = (state, state)

  def identity_state_vector : state_vector =
    map state_module.i64 (iota L.number_of_states)
    |> sized L.number_of_states

  def transitions [n] (str : [n]char) : [n]state_vector =
    map L.char_to_transitions str

  def minimum (set : terminal_set) : terminal =
    let m = bitset_u32.to_array set
        |> reduce_comm (i64.min) (L.number_of_terminals - 1)
    in terminal_module.i64 m

  def terminal_empty_set = bitset_u32.empty L.number_of_terminals

  def terminal_full_set = bitset_u32.complement terminal_empty_set

  type state_set = bitset_u32.bitset[(L.number_of_states - 1) / bitset_u32.nbs + 1]

  def state_empty_set = bitset_u32.empty L.number_of_states

  def state_full_set = bitset_u32.complement state_empty_set

  def compose_transition_vectors (a : state_vector) (b : state_vector) : state_vector =
    map (\s ->
      b[state_module.to_i64 s]
    ) a
  
  def find_ends [n] (state_vectors: [n]state_vector) : [n]state_set =
    map2 (\i v ->
      if i == n - 1
      then state_full_set
      else let w = state_vectors[i + 1]
           in map2 (\i s ->
                if w[state_module.to_i64 s] state_module.== L.dead_state then i else -1
              ) (indices v) v
              |> bitset_u32.from_array L.number_of_states
    ) (iota n) state_vectors

  def find_terminal_strings [n] (path : [n]transition) (str : [n]char) (ends : [n]bool) : [n]terminal_set =
    map3 (\is_end transition char ->
      let set = L.transition_to_terminal_set (transition, char)
      let final_set = copy L.inverted_final_terminal_states[state_module.to_i64 transition.1]
      in if is_end
         then final_set `bitset_u32.intersection` set
         else set
    ) ends path str

  def find_transition [n] (vectors : [n]state_vector) (composed_vectors : [n]state_vector) (ends : [n]state_set) (i : i64) : (transition, bool) =
    let next_index =
      composed_vectors[i]
      |> head
      |> state_module.to_i64
    let next_is_end = next_index `bitset_u32.member` ends[i + 1]
    let next_state = if next_is_end then L.initial_state else vectors[i + 1][next_index]
    let next_next_index = state_module.to_i64 next_state
    let next_next_state = vectors[i + 2][next_next_index]
    let next_next_is_end = next_next_index `bitset_u32.member` ends[i + 2]
    in ((next_state, next_next_state), next_next_is_end)

  def find_path [n] (state_vectors: [n]state_vector) : [n](transition, bool) =
    let vectors_with_start =
      replicate L.number_of_states L.initial_state
      |> replicate 2
      |> (++state_vectors)
    let ends = find_ends vectors_with_start
    let composed_vectors =
      tabulate_2d (2 + n) L.number_of_states (\i j ->
        let curr_state = vectors_with_start[i][j]
        in if j `bitset_u32.member` ends[i]
           then L.initial_state
           else curr_state
      )
      |> scan compose_transition_vectors identity_state_vector
    in tabulate n (find_transition vectors_with_start composed_vectors ends)

  def lex [n] (str : [n]char) : maybe ([](terminal, (i64, i64))) =
    let (path, ends) =
      transitions str
      |> find_path
      |> unzip
    let terminal_strings = find_terminal_strings path str ends
    let terminal_ends = filter (\i -> ends[i]) (iota n)
    let unfiltered_terminals_and_spans =
      map (\i ->
        let (a, b) =
          if i == 0
          then (0, terminal_ends[i] + 1)
          else (terminal_ends[i - 1] + 1, terminal_ends[i] + 1)
        let t = reduce_comm bitset_u32.intersection terminal_full_set terminal_strings[a:b]
                |> minimum
        in (t, (a, b))
      ) (indices terminal_ends)
    let terminals_and_spans = filter (not <-< L.is_ignore <-< (.0)) unfiltered_terminals_and_spans
    in if n == 0 ||
          (any (state_module.==path[n - 1].1) L.accepting_states &&
           all ((state_module.!=L.dead_state) <-< (.1)) path)
       then #just terminals_and_spans
       else #nothing
}

-- End of lexer.fut
