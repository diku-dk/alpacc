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
  val is_accepting : [state_size]bool
  val transitions_to_endomorphisms : [256][endomorphism_size]i64
  val transitions_to_keys : [256]i64
  val compositions : [endomorphism_size][endomorphism_size]i64
  val states_to_terminals : [state_size]i64
  val endomorphisms_to_states : [endomorphism_size]i64 
}

module mk_lexer(L: lexer_context) = {
  def lexer [n] (char : [n]u8) : [](i64, (i64, i64)) =
    []
}

-- End of lexer.fut
