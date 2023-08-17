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
        |> reduce_comm (i64.min) (L.number_of_terminals - 1)
    in terminal_module.i64 m

  def empty = bitset_u8.empty L.number_of_terminals

  def full_set = bitset_u8.complement empty

  def compose_transition_vectors [n] (a : [n](transition, bool)) (b : [n](transition, bool)) : [n](transition, bool) =
    map (\(a', is_end) ->
      if is_end
      then b[state_module.to_i64 L.initial_state]
      else b[state_module.to_i64 a'.1]
    ) a
  
  def compose_transition_vectors_identity =
    add_identity compose_transition_vectors
  
  def find_ends [n] (transition_vectors: [n](maybe transition_vector)) =
    map2 (\i v' ->
      map_maybe (sized L.transitions_size) <|
      let w = if i == n - 1 then #nothing else transition_vectors[i + 1]
      in match (v', w)
         case (#nothing, _) -> #nothing
         case (#just v, #nothing) -> map (\t -> (t, true)) v |> to_just
         case (#just v, #just w) ->
           map (\t ->
             (t, w[state_module.to_i64 t.1].1 state_module.== L.dead_state)
           ) v
           |> to_just
    ) (iota n) transition_vectors
  
  def find_terminal_strings [n] (path : [n]transition) (str : [n]char) (ends : [n]bool) : [n]bitset =
    zip path str
    |> map L.transition_to_terminal_set
    |> map3 (\is_end transition (set : bitset) ->
      let final_set = copy L.inverted_final_terminal_states[state_module.to_i64 transition.1]
      in if is_end
         then final_set `bitset_u8.intersection` set
         else set
    ) ends path

  def find_path [n] (transition_vectors: [n](maybe transition_vector)) : [1 + n](transition, bool) =
    [#just (replicate L.transitions_size (L.initial_state, L.initial_state))]
    |> (++transition_vectors)
    |> find_ends
    |> scan compose_transition_vectors_identity #nothing
    |> map (head <-< from_maybe (map (\t -> (t, true)) (copy L.dead_transitions)))

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
      reduce_comm bitset_u8.intersection full_set terminal_strings[i:j]
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
