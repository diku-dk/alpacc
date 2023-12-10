-- Start of lexer.fut
--
-- The generic parallel lexer, expressed as a parameterised
-- module.

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
    let a' = L.endomorphism_module.to_i64 a
    let b' = L.endomorphism_module.to_i64 b
    in copy L.compositions[b'][a']

  def is_accept (a : state) : bool =
    L.is_accepting[L.state_module.to_i64 a]

  def trans_to_endo [n] (str : [n]u8) (i : i64) : endomorphism =
    let c = u8.to_i64 str[i]
    let c' = u8.to_i64 str[i + 1]
    in copy L.transitions_to_endomorphisms[c'][c]
  
  def endo_to_state (e : endomorphism) : state =
    let e' = L.endomorphism_module.to_i64 e
    in copy L.endomorphisms_to_states[e']

  def lookup_state [n] (states : [n - 1]state) (str : [n]u8) (i : i64) : (bool, state) =
    let c = u8.to_i64 str[i]
    let prev_state =
      L.state_module.to_i64
      <| if i == 0 then L.initial_state else states[i - 1]
    let state = copy L.transitions_to_states[c][prev_state]
    let is_end = i == n - 1 || L.initial_state L.state_module.== states[i]
    in (is_end, state)

  def to_terminal (s : state) : terminal =
    let s' = L.state_module.to_i64 s
    in copy L.states_to_terminals[s']

  def to_ends_states [n] (str : [n]u8) : [n](bool, state) =
    let prev_states =
      tabulate (n - 1) (trans_to_endo str)
      |> scan compose L.identity_endomorphism
      |> map endo_to_state
    in tabulate n (lookup_state prev_states str)
    
  def lexer [n] (str : [n]u8) : [](terminal, (i64, i64)) =
    let ends_states = to_ends_states str
    let is_valid =
      all (\(e, s) -> not e || (e && is_accept s)) ends_states
    let is = filter (\i -> ends_states[i].0) (iota n)
    let new_size = length is
    in if is_valid
       then tabulate new_size (
                       \i ->
                         let start = if i == 0 then 0 else 1 + is[i - 1]
                         let end = is[i]
                         let span = (start, end + 1)
                         in (to_terminal ends_states[end].1, span)
                     )
            |> filter (not <-< L.is_ignore <-< (.0))
       else []
}

-- End of lexer.fut
