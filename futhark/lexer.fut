-- Start of lexer.fut
--
-- The generic parallel lexer, expressed as a parameterised
-- module.

import "lib/github.com/diku-dk/containers/opt"

module type lexer_context = {
  module endomorphism_module: integral
  module terminal_module: integral
  val identity_endomorphism : endomorphism_module.t
  val endomorphism_size : i64
  val endo_mask : endomorphism_module.t
  val terminal_mask : endomorphism_module.t
  val produce_mask : endomorphism_module.t
  val endo_offset : endomorphism_module.t
  val terminal_offset : endomorphism_module.t
  val produce_offset : endomorphism_module.t
  val ignore_terminal : opt terminal_module.t
  val transitions_to_endomorphisms : [256]endomorphism_module.t
  val compositions : [endomorphism_size * endomorphism_size]endomorphism_module.t
  val dead_terminal : terminal_module.t
  val accept_array : [endomorphism_size]bool
}

module type lexer = {
  type terminal
  val lex [n] : i32 -> [n]u8 -> opt ([](terminal, (i64, i64)))
}

module mk_lexer (L: lexer_context) : lexer with terminal = L.terminal_module.t = {
  type endomorphism = L.endomorphism_module.t
  type terminal = L.terminal_module.t

  def get_value (mask: endomorphism)
                (offset: endomorphism)
                (a: endomorphism) : endomorphism =
    let a' = mask L.endomorphism_module.& a
    in a' L.endomorphism_module.>> offset

  def is_produce (a: endomorphism) : bool =
    get_value L.produce_mask L.produce_offset a
    |> L.endomorphism_module.to_i64
    |> bool.i64

  def to_terminal (a: endomorphism) : terminal =
    get_value L.terminal_mask L.terminal_offset a
    |> L.endomorphism_module.to_i64
    |> L.terminal_module.i64

  def to_index (a: endomorphism) : i64 =
    get_value L.endo_mask L.endo_offset a
    |> L.endomorphism_module.to_i64

  def is_accept (a: endomorphism) : bool =
    L.accept_array[to_index a]

  def compose (a: endomorphism) (b: endomorphism) : endomorphism =
    #[unsafe]
    let a' = to_index a
    let b' = to_index b
    in copy L.compositions[b' * L.endomorphism_size + a']

  def trans_to_endo (prev_endo: endomorphism) (c: u8) (i: i64) : endomorphism =
    let e = copy L.transitions_to_endomorphisms[u8.to_i64 c]
    in if i == 0
       then prev_endo `compose` e
       else e

  def traverse [n] (prev_endo: endomorphism) (str: [n]u8) : *[n]endomorphism =
    map2 (trans_to_endo prev_endo) str (iota n)
    |> scan compose L.identity_endomorphism

  def take_right a b =
    if b == i64.highest then a else b

  def is_ignore t =
    match L.ignore_terminal
    case #some t' -> t L.terminal_module.== t'
    case #none -> false

  def lex_step [n]
               (offset: i64)
               (prev_endo: endomorphism)
               (prev_start: i64)
               (str: [n]u8) : ([](terminal, (i64, i64)), endomorphism, i64) =
    let endos = traverse prev_endo str
    let flags =
      tabulate n (\i ->
                    i != n - 1
                    && is_produce endos[i + 1]
                    && (not <-< is_ignore <-< to_terminal) endos[i])
    let is =
      map i64.bool flags
      |> scan (+) 0
    let offsets = map2 (\f o -> if f then o - 1 else -1) flags is
    let starts =
      tabulate n (\i ->
                    if is_produce endos[i] && (not <-< is_ignore <-< to_terminal) endos[i]
                    then i
                    else if i == 0 then take_right (prev_start - offset) i64.highest else i64.highest)
      |> scan take_right i64.highest
    let ends = iota n
    let vs = zip (map to_terminal endos) (zip starts ends)
    let dest = replicate n (L.terminal_module.u8 0, (0, 0))
    let result =
      scatter dest offsets vs
      |> map (\(t, (s, e)) -> (t, (s + offset, 1 + e + offset)))
    let size = is[n - 1]
    let last_endo = endos[n - 1]
    let last_start = starts[n - 1]
    in ( result[0:size]
       , last_endo
       , if last_start != i64.highest then offset + last_start else prev_start
       )

  def lex [n]
          (chunk_size: i32)
          (str: [n]u8) : opt ([](terminal, (i64, i64))) =
    let chunk_size' = i64.i32 chunk_size
    let (res, final_endo, final_start) =
      loop (res'', init_endo, init_start) = ([], L.identity_endomorphism, 0)
      for offset in 0..chunk_size'..<n do
        let m = i64.min (offset + chunk_size' + 1) n
        let (res', last_endo, last_start) = lex_step offset init_endo init_start str[offset:m]
        in (res'' ++ res', last_endo, last_start)
    let last_terminal = to_terminal final_endo
    let last =
      if is_ignore last_terminal
      then []
      else [(to_terminal final_endo, (final_start, n))]
    let result = some (res ++ last)
    in if is_accept final_endo
       then result
       else #none
}

-- End of lexer.fut
