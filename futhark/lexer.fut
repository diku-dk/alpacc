-- Start of lexer.fut
--
-- The generic parallel lexer, expressed as a parameterised
-- module.

import "lib/github.com/diku-dk/containers/opt"

module type lexer_context = {
  module state_module : integral
  module endomorphism_module : integral
  module terminal_module : integral
  val dead_state : state_module.t
  val initial_state : state_module.t
  val dead_endomorphism : endomorphism_module.t
  val identity_endomorphism : endomorphism_module.t
  val endomorphism_size : i64
  val state_size : i64
  val is_ignore : terminal_module.t -> bool
  val is_accepting : [state_size]bool
  val transitions_to_states : [256][state_size]state_module.t
  val transitions_to_endomorphisms : [256][256]endomorphism_module.t
  val compositions : [endomorphism_size][endomorphism_size]endomorphism_module.t
  val states_to_terminals : [state_size]terminal_module.t
  val endomorphisms_to_states : [endomorphism_size]state_module.t
}

module mk_lexer(L: lexer_context) = {
  type state = L.state_module.t
  type endomorphism = L.endomorphism_module.t
  type terminal = L.terminal_module.t

  def compose (a : endomorphism) (b : endomorphism) : endomorphism =
    #[unsafe]
    let a' = L.endomorphism_module.to_i64 a
    let b' = L.endomorphism_module.to_i64 b
    in copy L.compositions[b', a']

  def is_accept (a : state) : bool =
    L.is_accepting[L.state_module.to_i64 a]

  def trans_to_endo [n] (str : [n]u8) (i : i64) : endomorphism =
    if i == n - 1
    then L.identity_endomorphism
    else
      let c = u8.to_i64 str[i]
      let c' = u8.to_i64 str[i + 1]
      in copy L.transitions_to_endomorphisms[c', c]
  
  def endo_to_state (e : endomorphism) : state =
    let e' = L.endomorphism_module.to_i64 e
    in copy L.endomorphisms_to_states[e']

  def lookup_state [n] (endos : [n]endomorphism) (str : [n]u8) (i : i64) : (bool, state) =
    let c = u8.to_i64 str[i]
    let prev_state =
      L.state_module.to_i64
      <| if i == 0
         then L.initial_state
         else endo_to_state endos[i - 1]
    let state = copy L.transitions_to_states[c, prev_state]
    let pseudo_state = endo_to_state endos[i]
    let is_end = i == n - 1 || L.initial_state L.state_module.== pseudo_state
    in (is_end, state)

  def to_terminal (s : state) : terminal =
    let s' = L.state_module.to_i64 s
	in copy L.states_to_terminals[s']

  def to_ends_states [n] (str : [n]u8) : [n](bool, state) =
    let endos =
      tabulate n (trans_to_endo str)
      |> scan compose L.identity_endomorphism
    in tabulate n (lookup_state endos str)

  def lex [n] (str : [n]u8) : opt ([](terminal, (i64, i64))) =
    let ends_states = if n == 0 then [] else to_ends_states str
    let is = filter (\i -> ends_states[i].0) (iota n)
    let is_valid = all (\i -> is_accept ends_states[i].1) is
    let new_size = length is
    let result =
      tabulate new_size (
                 \i ->
                   let start = if i == 0 then 0 else 1 + is[i - 1]
                   let end = is[i]
                   let span = (start, end + 1)
                   in (to_terminal ends_states[end].1, span)
               )
      |> filter (not <-< L.is_ignore <-< (.0))
    in if is_valid
       then some result
       else #none

  def lex_step [n] (str : [n]u8) (offset : i64) (size : i64) : opt ([](terminal, (i64, i64)), i64) =
    let substr = str[offset: i64.min n (offset + size)]
    let m = length substr
    let ends_states = if m == 0 then [] else to_ends_states substr
    let is = filter (\i -> ends_states[i].0) (iota m)
    let new_size = length is
    let lexed' =
      tabulate new_size (
                 \i ->
                   let start = if i == 0 then 0 else 1 + is[i - 1]
                   let end = is[i]
                   let span = (offset + start, offset + end + 1)
                   in (ends_states[end].1, span)
               )
    let (lexed, new_offset') =
      if new_size <= 1 || m < size
      then ([], -1)
      else (init lexed', last lexed' |> (.1) |> (.0))
    let (states_spans, new_offset) =
      if m < size
      then (lexed', n - 1)
      else (lexed, new_offset')
    let is_invalid = all (\(s, _) -> is_accept s) states_spans |> not
    let terminals_spans =
      map (\(s, span) -> (to_terminal s, span)) states_spans
      |> filter (not <-< L.is_ignore <-< (.0))
    in if (new_size <= 1 && m == size) || is_invalid
       then #none
       else some (terminals_spans, new_offset)

  def lex' [n] (max_token_size : i64) (str : [n]u8) : opt ([](terminal, (i64, i64))) =
    let step = max_token_size + 1
    let (ys, final_offset, _) =
      loop (xs, offset, stop) = ([], 0, true) while stop do
        match lex_step str offset step
        case #none -> ([], -1, false)
        case #some (lexed, new_offset) ->
          let xs' = xs ++ lexed
          in if new_offset == n - 1
             then (xs', new_offset, false)
             else (xs', new_offset, true)
    in if final_offset == n - 1
       then some ys
       else #none
}

-- End of lexer.fut
