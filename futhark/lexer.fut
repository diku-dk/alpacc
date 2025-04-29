-- Start of lexer.fut
--
-- The generic parallel lexer, expressed as a parameterised
-- module.

import "lib/github.com/diku-dk/containers/opt"

module type lexer_context = {
  module endomorphism_module: integral
  module terminal_module: integral
  val identity_endomorphism: endomorphism_module.t
  val endomorphism_size: i64
  val endo_mask: endomorphism_module.t
  val terminal_mask: endomorphism_module.t
  val produce_mask: endomorphism_module.t
  val endo_offset: endomorphism_module.t
  val terminal_offset: endomorphism_module.t
  val produce_offset: endomorphism_module.t
  val is_ignore: terminal_module.t -> bool
  val transitions_to_endomorphisms: [256]endomorphism_module.t
  val compositions: [endomorphism_size * endomorphism_size]endomorphism_module.t
  val dead_terminal: terminal_module.t
  val accept_array: [endomorphism_size]bool
}

module mk_lexer(L: lexer_context) = {
  type endomorphism = L.endomorphism_module.t
  type terminal = L.terminal_module.t

  def get_value (mask: endomorphism)
                (offset: endomorphism)
                (a: endomorphism):
                endomorphism =
    let a' = mask L.endomorphism_module.& a
    in a' L.endomorphism_module.>> offset

  def is_produce (a: endomorphism): bool =
    get_value L.produce_mask L.produce_offset a
    |> L.endomorphism_module.to_i64
    |> bool.i64

  def to_terminal (a: endomorphism): terminal =
    get_value L.terminal_mask L.terminal_offset a
    |> L.endomorphism_module.to_i64
    |> L.terminal_module.i64

  def to_index (a: endomorphism): i64 =
    get_value L.endo_mask L.endo_offset a
    |> L.endomorphism_module.to_i64
                                    
  def is_accept (a: endomorphism): bool =
    L.accept_array[to_index a]
    
  def compose (a: endomorphism) (b: endomorphism): endomorphism =
    #[unsafe]
    let a' = to_index a
    let b' = to_index b
    in copy L.compositions[b' * L.endomorphism_size + a']
    
  def trans_to_endo (prev_endo: endomorphism) (c: u8) (i: i64): endomorphism =
    let e = copy L.transitions_to_endomorphisms[u8.to_i64 c]
    in
      if i == 0
      then prev_endo `compose` e
      else e

  def traverse [n] (prev_endo: endomorphism) (str: [n]u8): *[n]endomorphism =
    map2 (trans_to_endo prev_endo) str (iota n)
    |> scan compose L.identity_endomorphism
    
  def lex_step [n] (offset: i64)
                   (prev_endo: endomorphism)
                   (str: [n]u8):
                   ([](i64, terminal), endomorphism) =
    let endos = traverse prev_endo str
    let last_endo = endos[n - 1]
    let is = filter (\i -> not (i == 0 && offset == 0) && is_produce endos[i]) (0i64..<n)
    in (map (\i ->
               let e = if i == 0 then prev_endo else endos[i - 1]
               in (offset + i, to_terminal e)) is,
        last_endo)
  
  def lex [n]
          (chunk_size: i32)
          (str: [n]u8): opt ([](terminal, (i64, i64))) =
    let chunk_size' = i64.i32 chunk_size
    let (res, final_endo) =
      loop (res'', init_endo) = ([], L.identity_endomorphism) for offset in 0..chunk_size'..<n do
      let m = i64.min (offset + chunk_size') n
      let (res', last_endo) = lex_step offset init_endo str[offset:m]
      in (res'' ++ res', last_endo)
    let (final_starts, final_ters) = unzip res
    let final_ends = final_starts ++ [n]
    let final_starts = rotate (-1) <| final_starts ++ [0]
    let final_ters = final_ters ++ [to_terminal final_endo]
    let result =
      zip final_ters (zip final_starts final_ends)
      |> filter (not <-< L.is_ignore <-< (.0))
      |> some
    in if is_accept final_endo
       then result
       else #none 
}

-- End of lexer.fut
