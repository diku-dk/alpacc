-- Start of lexer.fut
--
-- The generic parallel lexer, expressed as a parameterised
-- module.

module type lexer_context = {
  val states_size: i64
  val accepting_states: [states_size]bool
  val compose : i64 -> i64 -> i64
  val endomorphism_to_state : i64 -> i64
  val transition_to_terminal : ((i64, i64), u8) -> i64
  val dead_endomorphism : i64
  val identity_endomorphism : i64
  val dead_state : i64
  val initial_state : i64
  val initial_loop_set : [256]bool
  val is_ignore : i64 -> bool
}

module mk_lexer(L: lexer_context) = {

  def to_token [n] (str : [n]u8) (states : [n]i64) (i : i64) =
    let chr = str[i]
    let next_chr = if i == n - 1 then 0 else str[i + 1]
    let prev = if i == 0 then L.initial_state else states[i - 1]
    let curr = states[i]
    let next = if i == n - 1 then L.dead_state else states[i + 1]
    let token = L.transition_to_terminal ((prev, curr), chr)
    let next_token = L.transition_to_terminal ((curr, next), next_chr)
    let is_end = n == n - 1 || token != next_token || not L.initial_loop_set[u8.to_i64 chr]
    let is_valid = !is_end || L.accepting_states[curr]
    in (is_valid, (is_end, i, token))
    
  def lexer [n] (str : [n]u8) =
    let states =
      str
      |> map (u8.to_i64)
      |> scan L.compose L.identity_endomorphism
      |> map L.endomorphism_to_state
    let (is_valid, labeled) = unzip <| tabulate n (to_token str states)
    let token_ends = filter (.0) labeled
    let unfilted_tokens =
      tabulate (length token_ends)
               (
                 \i ->
                   let (_, curr, token) = token_ends[i]
                   let prev = if i == 0 then 0 else 1 + token_ends[i - 1].1
                   in (token, (prev, 1 + curr))
               )
    let tokens = filter (not <-< L.is_ignore <-< (.0)) unfilted_tokens
    in if and is_valid then tokens else []
}

-- End of lexer.fut
