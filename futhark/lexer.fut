-- Start of lexer.fut
--
-- The generic parallel lexer, expressed as a parameterised
-- module.

module type lexer_context = {
  val dead_state : i64
  val initial_state : i64
  val dead_endomorphism : i64
  val identity_endomorphism : i64
  val endomorphism_size : i64
  val state_size : i64
  val is_ignore : i64 -> bool
  val is_accepting : [state_size]bool
  val transitions_to_endomorphisms : [256][state_size]i64
  val transitions_to_keys : [256][256]i64
  val compositions : [endomorphism_size][endomorphism_size]i64
  val states_to_terminals : [state_size]i64
  val endomorphisms_to_states : [endomorphism_size]i64 
}

module mk_lexer(L: lexer_context) = {
  def compose (a : i64) (b : i64) : i64 =
    L.compositions[b][a]

  def is_accept (a : i64) : bool =
    L.is_accepting[a]

  def trans_to_endo [n] (str : [n]u8) (i : i64) : i64 =
    let c = str[i]
    let c' = str[i + 1]
    in L.transitions_to_keys[u8.to_i64 c'][u8.to_i64 c]
  
  def endo_to_state (e : i64) : i64 =
    L.endomorphisms_to_states[e]

  def lookup_state [n] (states : [n - 1]i64) (str : [n]u8) (i : i64) : (bool, i64) =
    let c = u8.to_i64 str[i]
    let prev_state = if i == 0 then L.initial_state else states[i - 1]
    let state = L.transitions_to_endomorphisms[c][prev_state]
    let is_start = prev_state == L.initial_state
    in (is_start, state)

  def to_terminal (state : i64) : i64 =
    L.states_to_terminals[state]

  def lexer [n] (str : [n]u8) = -- : [](i64, (i64, i64)) =
    let prev_states =
      tabulate (n - 1) (trans_to_endo str)
      |> scan compose L.identity_endomorphism
      |> map endo_to_state
    let (starts, states) =
      tabulate n (lookup_state prev_states str)
      |> unzip
    let terminals = map to_terminal states
    let ends = rotate 1 starts
    let is_valid =
      map2 (\e s -> not e || (e && is_accept s)) ends states
      |> and
    let is = filter (\i -> ends[i]) (iota n)
    let new_size = length is
    in if is_valid
       then tabulate new_size (
                       \i ->
                         let start = if i == 0 then 0 else 1 + is[i - 1]
                         let end = is[i]
                         let span = (start, end + 1)
                         in (terminals[end], span)
                     )
            |> filter (not <-< L.is_ignore <-< (.0))
       else []
}

-- End of lexer.fut
