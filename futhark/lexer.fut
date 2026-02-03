-- Start of lexer.fut
--
-- The generic parallel lexer, expressed as a parameterised
-- module.

import "lib/github.com/diku-dk/containers/opt"

module type lexer_context = {
  module state_module: integral
  module terminal_module: integral
  val identity_state : state_module.t
  val state_size : i64
  val state_mask : state_module.t
  val terminal_mask : state_module.t
  val produce_mask : state_module.t
  val state_offset : state_module.t
  val terminal_offset : state_module.t
  val produce_offset : state_module.t
  val ignore_terminal : opt terminal_module.t
  val transitions_to_states : [256]state_module.t
  val compositions : [state_size * state_size]state_module.t
  val dead_terminal : terminal_module.t
  val accept_array : [state_size]bool
}

module type lexer = {
  type terminal
  val lex [n] : i32 -> [n]u8 -> opt ([](terminal, (i64, i64)))
}

module mk_lexer (L: lexer_context) : lexer with terminal = L.terminal_module.t = {
  type state = L.state_module.t
  type terminal = L.terminal_module.t

  def get_value (mask: state)
                (offset: state)
                (a: state) : state =
    let a' = mask L.state_module.& a
    in a' L.state_module.>> offset

  def is_produce (a: state) : bool =
    get_value L.produce_mask L.produce_offset a
    |> L.state_module.to_i64
    |> bool.i64

  def to_terminal (a: state) : terminal =
    get_value L.terminal_mask L.terminal_offset a
    |> L.state_module.to_i64
    |> L.terminal_module.i64

  def to_index (a: state) : i64 =
    get_value L.state_mask L.state_offset a
    |> L.state_module.to_i64

  def is_accept (a: state) : bool =
    L.accept_array[to_index a]

  def compose (a: state) (b: state) : state =
    #[unsafe]
    let a' = to_index a
    let b' = to_index b
    in copy L.compositions[b' * L.state_size + a']

  def trans_to_state (prev_state: state) (c: u8) (i: i64) : state =
    let e = copy L.transitions_to_states[u8.to_i64 c]
    in if i == 0
       then prev_state `compose` e
       else e

  def traverse [n] (prev_state: state) (str: [n]u8) : *[n]state =
    map2 (trans_to_state prev_state) str (iota n)
    |> scan compose L.identity_state

  def take_right a b =
    if b == i64.highest then a else b

  def is_ignore t =
    match L.ignore_terminal
    case #some t' -> t L.terminal_module.== t'
    case #none -> false

  def lex_step [m] [n]
               (offset: i64)
               (prev_state: state)
               (prev_start: i64)
               (prev_size: i64)
               (dest: *[m](terminal, (i64, i64)))
               (str: [n]u8) : ?[k].( [k](terminal, (i64, i64))
                                   , state
                                   , i64
                                   , i64
                                   ) =
    let states = traverse prev_state str
    let flags =
      tabulate n (\i ->
                    i != n - 1
                    && is_produce states[i + 1]
                    && (not <-< is_ignore <-< to_terminal) states[i])
    let is =
      map i64.bool flags
      |> scan (+) 0
    let offsets = map2 (\f o -> if f then o - 1 else -1) flags is
    let starts =
      tabulate n (\i ->
                    if is_produce states[i]
                    && (not <-< is_ignore <-< to_terminal) states[i]
                    then i
                    else if i == 0
                    then take_right (prev_start - offset) i64.highest
                    else i64.highest)
      |> scan take_right i64.highest
    let ends = iota n
    let vs = zip (map to_terminal states) (zip starts ends)
    let size = last is
    let extra_size = prev_size + size + 1
    let dest =
      if m <= extra_size
      then let new_dest =
             replicate (2 * extra_size) (L.terminal_module.u8 0, (0, 0))
           in scatter new_dest (indices dest) dest
      else dest
    let result =
      scatter dest (map (+ prev_size) offsets) vs
      |> map (\(t, (s, e)) -> (t, (s + offset, 1 + e + offset)))
    let last_state = last states
    let last_start = last starts
    in ( result
       , last_state
       , if last_start != i64.highest
         then offset + last_start
         else prev_start
       , prev_size + size
       )

  def lex [n]
          (chunk_size: i32)
          (str: [n]u8) : opt ([](terminal, (i64, i64))) =
    let chunk_size = i64.i32 chunk_size
    let (result, state, start, size) =
      loop (dest, state, start, size) =
             ( [(L.terminal_module.u8 0, (0, 0))]
             , L.identity_state
             , 0
             , 0
             )
      for offset in 0..chunk_size..<n do
        let m = i64.min (offset + chunk_size + 1) n
        let s = copy str[offset:m]
        let (dest, last_state, last_start, size) =
          lex_step offset state start size dest s
        in (dest, copy last_state, last_start, size)
    let last_terminal = to_terminal state
    let (result, size) =
      if is_ignore last_terminal
      then (result, size)
      else ( result with [size] = ( to_terminal state
                                  , (start, n)
                                  )
           , size + 1
           )
    in if is_accept state
       then some (take size result)
       else #none
}

-- End of lexer.fut
