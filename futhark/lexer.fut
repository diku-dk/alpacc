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
  val accept_mask: endomorphism_module.t
  val produce_mask: endomorphism_module.t
  val endo_offset: endomorphism_module.t
  val terminal_offset: endomorphism_module.t
  val accept_offset: endomorphism_module.t
  val produce_offset: endomorphism_module.t
  val is_ignore: terminal_module.t -> bool
  val transitions_to_endomorphisms: [256]endomorphism_module.t
  val compositions: [endomorphism_size][endomorphism_size]endomorphism_module.t
  val dead_terminal: terminal_module.t
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
                                    
  def is_accept (a: endomorphism): bool =
    get_value L.accept_mask L.accept_offset a
    |> L.endomorphism_module.to_i64
    |> bool.i64

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
    
  def compose (a: endomorphism) (b: endomorphism): endomorphism =
    #[unsafe]
    let a' = to_index a
    let b' = to_index b
    in copy L.compositions[b', a']
    
  def trans_to_endo (c: u8): endomorphism =
    copy L.transitions_to_endomorphisms[u8.to_i64 c]

  def traverse [n] (str: [n]u8): *[n](bool, endomorphism) =
    let endos =
      map trans_to_endo str
      |> scan compose L.identity_endomorphism
    let produces = map is_produce endos |> rotate 1
    let produces =
      if n == 0
      then produces
      else let produces[n - 1] = true in produces
    in zip produces endos

  def lex_with_dead [n'] (offset: i32)
                         (str: [n']u8):
                         [](terminal, (i32, i32)) =
    let n = i32.i64 n'
    let ends_endos = if n == 0 then [] else traverse str
    let is = filter (\i -> ends_endos[i].0) (0i32..<n)
    let new_size = length is
    in tabulate new_size (
                  \i ->
                    let start = if i == 0 then 0 else 1 + is[i - 1]
                    let end = is[i]
                    let span = (offset + start, offset + end + 1)
                    let (_, endo) = ends_endos[end]
                    in (to_terminal endo, span)
                )
    
  def lex [n'] (str: [n']u8): opt ([](terminal, (i32, i32))) =
    let result = lex_with_dead 0 str
    let is_valid =
      length result == 0 ||
      (last result).0 L.terminal_module.!= L.dead_terminal
    let result = filter (not <-< L.is_ignore <-< (.0)) result
    in if is_valid
       then some result
       else #none

  def lex_step [n']
               (str: [n']u8)
               (offset: i32)
               (size: i32): opt ([](terminal, (i32, i32))) =
    let n = i32.i64 n'
    let end = i32.min n (offset + size)
    let substr = str[i64.i32 offset: i64.i32 end]
    let result = lex_with_dead offset substr
    let result =
      if length result == 0 || end == n
      then result
      else init result
    in if length result == 0 ||
          (last result).0 L.terminal_module.== L.dead_terminal
       then #none
       else #some result

  def lex_chunked [n']
                  (max_token_size: i32)
                  (str: [n']u8): opt ([](terminal, (i32, i32))) =
    let n = i32.i64 n'
    let step = max_token_size
    let (ys, final_offset, _) =
      loop (xs, offset, continue) = ([], 0, true) while continue do
        match lex_step str offset step
        case #none -> ([], -1, false)
        case #some lexed ->
        let (_, (_, m)) = last lexed
        let xs' = xs ++ filter (not <-< L.is_ignore <-< (.0)) lexed
        in (xs', m, m != n)
    in if final_offset != -1 || n == 0
       then #some ys
       else #none
}

-- End of lexer.fut
