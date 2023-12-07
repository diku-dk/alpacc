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
  val transitions_to_endomorphisms : [256][state_size]i64
  val transitions_to_keys : [256]i64
  val compositions : [endomorphism_size][endomorphism_size]i64
  val states_to_terminals : [state_size]i64
  val endomorphisms_to_states : [endomorphism_size]i64 
}

module mk_lexer(L: lexer_context) = {
  def compose (a : i64) (b : i64) : i64 =
    L.compositions[a][b]

  def trans_to_endo_key (c : u8) : i64 =
    L.transitions_to_keys[u8.to_i64 c]

  def endo_to_state (i : i64) : i64 =
    L.endomorphisms_to_states[i]
  
  def lexer [n] (str : [n]u8) = -- : [](i64, (i64, i64)) =
    map trans_to_endo_key str
    |> scan compose L.identity_endomorphism
    |> map endo_to_state
}

-- End of lexer.fut
