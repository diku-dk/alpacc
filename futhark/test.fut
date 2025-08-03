-- Start of test.fut
--
-- The generic parallel lexer tester, expressed as a parameterised
-- module.

import "lib/github.com/diku-dk/containers/opt"

def encode_u64 (a: u64) : [8]u8 =
  [ u8.u64 (a >> 56)
  , u8.u64 (a >> 48)
  , u8.u64 (a >> 40)
  , u8.u64 (a >> 32)
  , u8.u64 (a >> 24)
  , u8.u64 (a >> 16)
  , u8.u64 (a >> 8)
  , u8.u64 (a >> 0)
  ]

def decode_u64 (a: [8]u8) : u64 =
  (u64.u8 a[7] << 56)
  | (u64.u8 a[6] << 48)
  | (u64.u8 a[5] << 40)
  | (u64.u8 a[4] << 32)
  | (u64.u8 a[3] << 24)
  | (u64.u8 a[2] << 16)
  | (u64.u8 a[1] << 8)
  | (u64.u8 a[0] << 0)

module lexer_test
  (L: {
    type terminal
    val lex [n] : i32 -> [n]u8 -> opt ([](terminal, (i64, i64)))
  })
  (T: integral with t = L.terminal) = {
  type terminal = L.terminal

  def encode_terminal ((t, (i, j)): (terminal, (i64, i64))) : [24]u8 =
    sized 24 (encode_u64 (T.to_i64 t |> u64.i64)
              ++ encode_u64 (u64.i64 i)
              ++ encode_u64 (u64.i64 j))

  def encode_terminals [n] (ts: opt ([n](terminal, (i64, i64)))) : []u8 =
    match ts
    case #some ts' ->
      [u8.bool true]
      ++ encode_u64 (u64.i64 n)
      ++ flatten (map encode_terminal ts')
    case #none -> [u8.bool false]

  def test [n] (chunk_size: i32) (bytes: [n]u8) : []u8 =
    let num = take 8 bytes
    let num_tests = decode_u64 num
    let (a, _) =
      loop (result, inputs) = (num, drop 8 bytes)
      for _i < u64.to_i64 num_tests do
        let input_size = u64.to_i64 (decode_u64 (take 8 inputs))
        let inputs' = drop 8 inputs
        let input = take input_size inputs'
        let inputs'' = drop input_size inputs'
        let output = L.lex chunk_size input |> encode_terminals
        in (result ++ output, inputs'')
    in a
}

module parser_test
  (P: {
    type terminal
    type production
    val pre_productions [n] : [n]terminal -> opt ([]production)
  })
  (T: integral with t = P.terminal)
  (Q: integral with t = P.production) = {
  type terminal = P.terminal
  type production = P.production

  def test [n] (bytes: [n]u8) : []u8 =
    let num = take 8 bytes
    let num_tests = decode_u64 num
    let (a, _) =
      loop (result, inputs) = (num, drop 8 bytes)
      for _i < u64.to_i64 num_tests do
        let input_size = u64.to_i64 (decode_u64 (take 8 inputs))
        let inputs' = drop 8 inputs
        let input =
          take (input_size * 8) inputs'
          |> unflatten
          |> map (T.u64 <-< decode_u64)
        let inputs'' = drop input_size inputs'
        let output =
          P.pre_productions input
          |> map (encode_u64 <-< u64.i64 <-< Q.to_i64)
          |> flatten
        in (result ++ output, inputs'')
    in a
}

-- End of test.fut
