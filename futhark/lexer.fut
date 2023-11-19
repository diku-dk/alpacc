-- Start of lexer.fut
--
-- The generic parallel lexer, expressed as a parameterised
-- module.

module type lexer_context = {
  val compose : i64 -> i64 -> i64
  val endomorphism_to_state : i64 -> i64
  val transition_to_terminal : ((i64, i64), u8) -> i64
  val dead_endomorphism : i64
  val identity_endomorphism : i64
  val dead_state : i64
  val initial_state : i64
}

module mk_lexer(L: lexer_context) = {
  def test [n] (str : [n]u8) : [n]i64 =
    let temp =
      str
      |> map (u8.to_i64)
      |> scan L.compose L.identity_endomorphism
      |> map L.endomorphism_to_state
    in tabulate n (\i -> L.transition_to_terminal ((if i == 0 then L.initial_state else temp[i - 1], temp[i]), str[i]))
}

-- End of lexer.fut
