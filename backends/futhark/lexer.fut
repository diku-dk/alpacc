-- Start of lexer.fut
--
-- The generic parallel lexer, expressed as a parameterised
-- module.

import "lib/github.com/diku-dk/containers/core/opt"

def chunk_size : i64 = #[param(chunk_size)] 16777216i64

module type lexer_context = {
  type terminal
  module state_module: integral
  module terminal_int_module: integral
  val identity_state : state_module.t
  val state_size : i64
  val state_mask : state_module.t
  val terminal_mask : state_module.t
  val produce_mask : state_module.t
  val state_offset : state_module.t
  val terminal_offset : state_module.t
  val produce_offset : state_module.t
  val ignore_terminal : opt terminal_int_module.t
  val transitions_to_states : [256]state_module.t
  val compositions : [state_size * state_size]state_module.t
  val dead_terminal : terminal_int_module.t
  val accept_array : [state_size]bool
  val number_of_terminals : i64
  val terminal_int_to_name : [number_of_terminals]terminal
}

module type lexer = {
  type terminal_int
  type terminal
  val lex_int [n] : [n]u8 -> opt ([](terminal_int, (idx.t, len.t)))
  val lex [n] : [n]u8 -> opt ([](terminal, (idx.t, len.t)))
}

module mk_lexer (L: lexer_context)
  : lexer
    with terminal_int = L.terminal_int_module.t
    with terminal = L.terminal = {
  type terminal = L.terminal
  type state = L.state_module.t
  type terminal_int = L.terminal_int_module.t

  def get_value (mask: state)
                (offset: state)
                (a: state) : state =
    let a' = mask L.state_module.& a
    in a' L.state_module.>> offset

  def is_produce (a: state) : bool =
    get_value L.produce_mask L.produce_offset a
    |> L.state_module.to_i64
    |> bool.i64

  def to_terminal (a: state) : terminal_int =
    get_value L.terminal_mask L.terminal_offset a
    |> L.state_module.to_i64
    |> L.terminal_int_module.i64

  def to_index (a: state) : i64 =
    get_value L.state_mask L.state_offset a
    |> L.state_module.to_i64

  def is_accept (a: state) : bool =
    L.accept_array[to_index a]

  def compose (a: state) (b: state) : state =
    #[unsafe]
    let a' = to_index a
    let b' = to_index b
    in copy L.compositions[a' * L.state_size + b']

  -- `first` is true for the first chunk of the input.  For later chunks the
  -- first byte is the one-byte overlap with the previous chunk: its
  -- transition is already part of prev_state, so composing it again would
  -- apply the byte twice (harmless only for idempotent transitions such as
  -- digit runs, wrong for e.g. keywords).  The seed of the scan is then
  -- prev_state itself.
  def trans_to_state (first: bool) (prev_state: state) (c: u8) (i: i64) : state =
    let e = copy L.transitions_to_states[u8.to_i64 c]
    in if i == 0
       then if first then prev_state `compose` e else prev_state
       else e

  def traverse [n] (first: bool) (prev_state: state) (str: [n]u8) : *[n]state =
    map2 (trans_to_state first prev_state) str (iota n)
    |> scan compose L.identity_state

  def is_ignore t =
    match L.ignore_terminal
    case #some t' -> t L.terminal_int_module.== t'
    case #none -> false

  def lex_step [m] [n]
               (offset: idx.t)
               (prev_state: state)
               (prev_start: idx.t)
               (prev_size: idx.t)
               (dest: *[m](terminal_int, (idx.t, len.t)))
               (str: [n]u8) : ?[k].( *[k](terminal_int, (idx.t, len.t))
                                   , state
                                   , idx.t
                                   , idx.t
                                   ) =
    let states = traverse (offset == idx.i64 0) prev_state str
    let flags =
      tabulate n (\i ->
                    i != n - 1
                    && is_produce states[i + 1]
                    && (not <-< is_ignore <-< to_terminal) states[i])
    let is =
      map i64.bool flags
      |> scan (+) 0
    let offsets = map2 (\f o -> if f then o - 1 else -1) flags is
    -- Token starts as a max-scan: idx.lowest = "no start seen"; injected
    -- values are non-decreasing (boundary seed <= 0 <= produce indices), so
    -- max is equivalent to take-rightmost-non-sentinel.
    let starts =
      tabulate n (\i ->
                    if is_produce states[i]
                    && (not <-< is_ignore <-< to_terminal) states[i]
                    then idx.i64 i
                    else if i == 0
                    then prev_start - offset
                    else idx.lowest)
      |> scan idx.max idx.lowest
    -- Globalise spans here, before the scatter: mapping the scatter result
    -- instead would re-add this chunk's offset to tokens already emitted by
    -- earlier chunks.
    -- Length = (end_exclusive) - start = (i + 1) - starts[i]; starts[i] is
    -- the index of the produce boundary, so this is always >= 1.
    -- Non-boundary positions (starts[i] == idx.lowest) get length 0 as a
    -- placeholder; they are never written by the scatter (offsets[i] == -1).
    let vs =
      zip (map to_terminal states) starts
      |> map2 (\i (t, s) ->
                 let l = if s == idx.lowest
                         then len.i64 0
                         else len.i64 (i + 1 - idx.to_i64 s)
                 in (t, (s + offset, l))) (iota n)
    let size = last is
    let extra_size = idx.to_i64 prev_size + size + 1
    let dest =
      if m <= extra_size
      then let new_dest =
             replicate (2 * extra_size) (L.terminal_int_module.u8 0, (idx.i64 0, len.i64 0))
           in scatter new_dest (indices dest) dest
      else dest
    -- The -1 entries of `offsets` mark positions with no token; they must
    -- stay negative (out of bounds, so ignored by scatter).  Adding
    -- `prev_size` unconditionally would turn them into `prev_size - 1` and
    -- clobber the previous chunk's last token.
    let result =
      scatter dest
              (map (\o -> if o < 0 then -1 else o + idx.to_i64 prev_size) offsets)
              vs
    let last_state = last states
    let last_start = last starts
    in ( result
       , last_state
       , if last_start != idx.lowest
         then offset + last_start
         else prev_start
       , idx.i64 (idx.to_i64 prev_size + size)
       )

  def lex_int_flag [n]
                   (str: [n]u8) : (bool, [](terminal_int, (idx.t, len.t))) =
    let chunk_size = idx.i64 chunk_size
    let (result, state, start, size) =
      loop (dest, state, start, size) =
             ( [(L.terminal_int_module.u8 0, (idx.i64 0, len.i64 0))]
             , L.identity_state
             , idx.i64 0
             , idx.i64 0
             )
      for offset in 0..idx.to_i64 chunk_size..<n do
        let m = i64.min (offset + idx.to_i64 chunk_size + 1) n
        let s = copy str[offset:m]
        let (dest, last_state, last_start, size) =
          lex_step (idx.i64 offset) state start size dest s
        in (dest, copy last_state, last_start, size)
    let last_terminal = to_terminal state
    let (result, size) =
      if is_ignore last_terminal
      then (result, size)
      else ( result with [idx.to_i64 size] = ( to_terminal state
                                              , (start, len.i64 (n - idx.to_i64 start))
                                              )
           , size + idx.i64 1
           )
    in if is_accept state
       then (true, take (idx.to_i64 size) result)
       else (false, [])

  def lex_int [n]
              (str: [n]u8) : opt ([](terminal_int, (idx.t, len.t))) =
    let (is_valid, result) = lex_int_flag str
    in if is_valid then #some result else #none

  def lex [n]
          (str: [n]u8) : opt ([](terminal, (idx.t, len.t))) =
    let (is_valid, result) = lex_int_flag str
    in if is_valid
       then #some (map (\(t, s) ->
                          ( copy L.terminal_int_to_name[L.terminal_int_module.to_i64 t]
                          , s
                          ))
                       result)
       else #none
}

-- End of lexer.fut
