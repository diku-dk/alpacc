-- Start of test.fut
--
-- The generic parallel test harness, expressed as parameterised
-- modules.
--
-- Wire format (native little-endian batch protocol, see
-- docs/wire-protocols.md):
--   input : u64 num_tests, then per test a request frame:
--           u64 frame_len, u64 n, payload
--           (payload: n raw bytes for lexer/combined;
--            n × sizeof(terminal) token ids for parser)
--   output: u64 num_tests, then per test a response record:
--           u8 valid; if valid: u64 count, then native-width fields.
-- All integers are little-endian; token/production ids use the
-- grammar's native widths, starts and parents are sizeof(idx.t) bytes
-- (2's-complement signed), lengths use the grammar's len.t width.

import "lib/github.com/diku-dk/containers/core/opt"

#[inline]
def encode_le (num_bytes: i64) (v: u64) : [num_bytes]u8 =
  tabulate num_bytes (\i -> u8.u64 (v >> (8 * u64.i64 i)))

#[inline]
def decode_le [n] (bytes: [n]u8) : u64 =
  loop acc = 0u64
  for i < n do
    acc | (u64.u8 bytes[i] << (8 * u64.i64 i))

#[inline]
def encode_u64 (a: u64) : [8]u8 = encode_le 8 a

#[inline]
def decode_u64 (a: [8]u8) : u64 = decode_le a

module lexer_test
  (L: {
    type terminal_int
    val lex_int [n] : [n]u8 -> opt ([](terminal_int, (idx.t, len.t)))
  })
  (T: integral with t = L.terminal_int) = {
  type terminal = L.terminal_int

  def terminal_bytes : i64 = i64.i32 T.num_bits / 8
  def length_bytes   : i64 = i64.i32 len.num_bits / 8
  def idx_bytes      : i64 = i64.i32 idx.num_bits / 8
  def lexeme_bytes   : i64 = terminal_bytes + idx_bytes + length_bytes

  #[inline]
  def encode_terminal ((t, (i, j)): (terminal, (idx.t, len.t))) : [lexeme_bytes]u8 =
    sized lexeme_bytes (encode_le terminal_bytes (u64.i64 (T.to_i64 t))
                        ++ encode_le idx_bytes    (u64.i64 (idx.to_i64 i))
                        ++ encode_le length_bytes (u64.i64 (len.to_i64 j)))

  #[inline]
  def encode_terminals [n] (ts: opt ([n](terminal, (idx.t, len.t)))) : []u8 =
    match ts
    case #some ts' ->
      [u8.bool true]
      ++ encode_u64 (u64.i64 n)
      ++ flatten (map encode_terminal ts')
    case #none -> [u8.bool false]

  def test [n] (bytes: [n]u8) : []u8 =
    let num = take 8 bytes
    let num_tests = decode_u64 num
    let (a, _, size) =
      loop (result, inputs, size) = copy (num, drop 8 bytes, length num)
      for _i < u64.to_i64 num_tests do
        -- Skip the u64 frame_len; the u64 n that follows determines the size.
        let inputs' = drop 8 inputs
        let input_size = u64.to_i64 (decode_u64 (take 8 inputs'))
        let inputs'' = drop 8 inputs'
        let input = take input_size inputs''
        let inputs''' = drop input_size inputs''
        let output = L.lex_int input |> encode_terminals
        let new_size = size + length output
        let result =
          if length result <= new_size
          then scatter (replicate (2 * new_size) 0) (indices result) result
          else result
        let result = scatter result (map (+ size) (indices output)) output
        in (result, inputs''', new_size)
    in take size a
}

module parser_test
  (P: {
    type terminal_int
    type production_int
    val pre_productions_int [n] : [n]terminal_int -> opt ([]production_int)
  })
  (T: integral with t = P.terminal_int)
  (Q: integral with t = P.production_int) = {
  type terminal = P.terminal_int
  type production = P.production_int

  def terminal_bytes : i64 = i64.i32 T.num_bits / 8
  def production_bytes : i64 = i64.i32 Q.num_bits / 8

  #[inline]
  def encode_productions [n] (ts: opt ([n]production)) : []u8 =
    match ts
    case #some ts' ->
      [u8.bool true]
      ++ encode_u64 (u64.i64 n)
      ++ flatten (map (encode_le production_bytes <-< u64.i64 <-< Q.to_i64) ts')
    case #none -> [u8.bool false]

  def test [n] (bytes: [n]u8) : []u8 =
    let num = take 8 bytes
    let num_tests = decode_u64 num
    let (a, _, size) =
      loop (result, inputs, size) = copy (num, drop 8 bytes, length num)
      for _i < u64.to_i64 num_tests do
        -- Skip the u64 frame_len; the u64 n that follows determines the size.
        let inputs' = drop 8 inputs
        let input_size = u64.to_i64 (decode_u64 (take 8 inputs'))
        let inputs'' = drop 8 inputs'
        let payload = take (input_size * terminal_bytes) inputs''
        let input =
          tabulate input_size
                   (\q -> T.u64 (decode_le payload[q * terminal_bytes:(q + 1) * terminal_bytes]))
        let inputs''' = drop (input_size * terminal_bytes) inputs''
        let output =
          P.pre_productions_int input
          |> encode_productions
        let new_size = size + length output
        let result =
          if length result <= new_size
          then scatter (replicate (2 * new_size) 0) (indices result) result
          else result
        let result = scatter result (map (+ size) (indices output)) output
        in (result, inputs''', new_size)
    in take size a
}

module lexer_parser_test
  (P: {
    type terminal_int
    type production_int
    val parse_int [n] : [n]u8 -> opt ( [](terminal_int, (idx.t, len.t))
                                     , [](production_int, idx.t)
                                     , []idx.t
                                     )
  })
  (T: integral with t = P.terminal_int)
  (Q: integral with t = P.production_int) = {
  type terminal = P.terminal_int
  type production = P.production_int

  def terminal_bytes  : i64 = i64.i32 T.num_bits / 8
  def production_bytes: i64 = i64.i32 Q.num_bits / 8
  def length_bytes    : i64 = i64.i32 len.num_bits / 8
  def idx_bytes       : i64 = i64.i32 idx.num_bits / 8

  #[inline]
  def encode_idx (i: idx.t) : [idx_bytes]u8 =
    encode_le idx_bytes (u64.i64 (idx.to_i64 i))

  -- SoA response record: u8 valid; u64 num_tokens; token ids, starts,
  -- lengths; u64 num_nodes; production ids, parents; token parents.
  #[inline]
  def encode_result (r: opt ( [](terminal, (idx.t, len.t))
                            , [](production, idx.t)
                            , []idx.t
                            )) : []u8 =
    match r
    case #some (tokens, tree, token_parents) ->
      let (ts, spans) = unzip tokens
      let (starts, lengths) = unzip spans
      let (prods, parents) = unzip tree
      in [u8.bool true]
         ++ encode_u64 (u64.i64 (length tokens))
         ++ flatten (map (encode_le terminal_bytes <-< u64.i64 <-< T.to_i64) ts)
         ++ flatten (map encode_idx starts)
         ++ flatten (map (encode_le length_bytes <-< u64.i64 <-< len.to_i64) lengths)
         ++ encode_u64 (u64.i64 (length tree))
         ++ flatten (map (encode_le production_bytes <-< u64.i64 <-< Q.to_i64) prods)
         ++ flatten (map encode_idx parents)
         ++ flatten (map encode_idx token_parents)
    case #none -> [u8.bool false]

  def test [n] (bytes: [n]u8) : []u8 =
    let num = take 8 bytes
    let num_tests = decode_u64 num
    let (a, _, size) =
      loop (result, inputs, size) = copy (num, drop 8 bytes, length num)
      for _i < u64.to_i64 num_tests do
        -- Skip the u64 frame_len; the u64 n that follows determines the size.
        let inputs' = drop 8 inputs
        let input_size = u64.to_i64 (decode_u64 (take 8 inputs'))
        let inputs'' = drop 8 inputs'
        let input = take input_size inputs''
        let inputs''' = drop input_size inputs''
        let output =
          P.parse_int input
          |> encode_result
        let new_size = size + length output
        let result =
          if length result <= new_size
          then scatter (replicate (2 * new_size) 0) (indices result) result
          else result
        let result = scatter result (map (+ size) (indices output)) output
        in (result, inputs''', new_size)
    in take size a
}

-- End of test.fut
