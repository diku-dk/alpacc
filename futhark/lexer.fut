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
  val continue_terminal_states : [number_of_terminals](bitset_u32.bitset[(number_of_states - 1) / bitset_u32.nbs + 1])
  val inverted_final_terminal_states : [number_of_states](bitset_u32.bitset[(number_of_terminals - 1) / bitset_u32.nbs + 1])
  val inverted_continue_terminal_states : [number_of_states](bitset_u32.bitset[(number_of_terminals - 1) / bitset_u32.nbs + 1])
  val char_to_transitions : char_module.t -> [number_of_states](maybe state_module.t)
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

  type bitset = bitset_u32.bitset[(L.number_of_terminals - 1) / bitset_u32.nbs + 1]
  type state_vector = [L.number_of_states]state
  type maybe_state_vector = [L.number_of_states](maybe state)

  type transition = (state, state)

  def identity_state_vector : state_vector =
    map state_module.i64 (iota L.number_of_states)
    |> sized L.number_of_states
  
  def identity_maybe_state_vector : maybe_state_vector =
    replicate L.number_of_states #nothing

  def combine (_ : state) (a : state) : state =
    a

  def combine_transitions (a : maybe_state_vector) (b : maybe_state_vector) : maybe_state_vector =
    map2 (add_identity combine) a b

  def solve_transitions [n] (arr : [n]maybe_state_vector) : [n]state_vector =
    scan combine_transitions identity_maybe_state_vector arr
    |> map (map (from_maybe L.dead_state))

  def transitions [n] (str : [n]char) : [n]maybe_state_vector =
    map L.char_to_transitions str

  def minimum (set : bitset) : terminal =
    let m = bitset_u32.to_array set
        |> reduce_comm (i64.min) (L.number_of_terminals - 1)
    in terminal_module.i64 m

  def empty = bitset_u32.empty L.number_of_terminals

  def full_set = bitset_u32.complement empty

  def compose_transition_vectors (a : state_vector) (b : state_vector) : state_vector =
    map (\s ->
      b[state_module.to_i64 s]
    ) a
  
  def find_ends [n] (state_vectors: [n]state_vector) : [n][L.number_of_states]bool =
    map2 (\i v ->
      if i == n - 1
      then replicate L.number_of_states true
      else let w = state_vectors[i + 1]
           in map (\s ->
                w[state_module.to_i64 s] state_module.== L.dead_state
              ) v
              |> sized L.number_of_states
    ) (iota n) state_vectors

  def find_terminal_strings [n] (path : [n]transition) (str : [n]char) (ends : [n]bool) : [n]bitset =
    zip path str
    |> map L.transition_to_terminal_set
    |> map3 (\is_end transition (set : bitset) ->
      let final_set = copy L.inverted_final_terminal_states[state_module.to_i64 transition.1]
      in if is_end
         then final_set `bitset_u32.intersection` set
         else set
    ) ends path

  def f [n] (state_vectors : [n]state_vector) (a : (i64, state)) (b : (i64, state)) =
    (b.0, state_vectors[a.0][state_module.to_i64 b.1])

  def find_path [n] (state_vectors: [n]state_vector) = --: [1 + n](transition, bool) =
    let vectors_with_start =
      [replicate L.number_of_states L.initial_state]
      |> (++state_vectors)
    let ends = find_ends vectors_with_start
    let composed_states =
      tabulate_2d (1 + n) L.number_of_states (\i j ->
        let curr_state = vectors_with_start[i][j]
        in if ends[i][j]
           then L.initial_state
           else curr_state
      )
      |> scan compose_transition_vectors identity_state_vector
      |> map head
    let states =
      map (\i ->
        let curr_index = state_module.to_i64 composed_states[i]
        let next_state = vectors_with_start[i + 1][curr_index]
        let is_end = ends[i + 1][curr_index]
        in (next_state, is_end)
      ) (iota n)
      |> ([(L.initial_state, false)]++)
    in tabulate (1 + n) (\i ->
      let (prev', b') = states[i64.max 0 (i - 1)]
      let prev = if b' then L.initial_state else prev'
      let (next, b) = states[i]
      in ((prev, next), b)
    )

  def lex [n] (str : [n]char) : maybe ([]terminal, [](i64, i64)) =
    let (path_with_init, ends_with_init) =
      transitions str
      |> solve_transitions
      |> find_path
      |> unzip
    let path = tail path_with_init |> sized n
    let ends = tail ends_with_init |> sized n
    let terminal_strings = find_terminal_strings path str ends
    let terminal_ends =
      zip (iota n) ends
      |> filter (\(_, b) -> b)
      |> map (.0)
    let unfiltered_spans =
      map (\i ->
        if i == 0
        then (0, terminal_ends[i] + 1)
        else (terminal_ends[i - 1] + 1, terminal_ends[i] + 1)
      ) (indices terminal_ends)
    let unfiltered_terminals = map (\(i, j) ->
      reduce_comm bitset_u32.intersection full_set terminal_strings[i:j]
    ) unfiltered_spans
    |> map minimum
    let terminal_indices = 
      filter (\i -> not (L.is_ignore unfiltered_terminals[i])) (indices unfiltered_spans)
    let spans = map (\i -> unfiltered_spans[i]) terminal_indices
    let terminals = map (\i -> unfiltered_terminals[i]) terminal_indices
    in if (any (state_module.==path_with_init[n].1) L.accepting_states &&
          all ((state_module.!=L.dead_state) <-< (.1)) path_with_init) ||
          n == 0
       then #just (terminals, spans)
       else #nothing
}

-- End of lexer.fut
