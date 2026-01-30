-- Start of test.fut
--
-- The generic parallel lexer tester, expressed as a parameterised
-- module.

import "lib/github.com/diku-dk/containers/opt"

#[inline]
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

#[inline]
def decode_u64 (a: [8]u8) : u64 =
  (u64.u8 a[0] << 56)
  | (u64.u8 a[1] << 48)
  | (u64.u8 a[2] << 40)
  | (u64.u8 a[3] << 32)
  | (u64.u8 a[4] << 24)
  | (u64.u8 a[5] << 16)
  | (u64.u8 a[6] << 8)
  | (u64.u8 a[7] << 0)

module lexer_test
  (L: {
    type terminal
    val lex [n] : i32 -> [n]u8 -> opt ([](terminal, (i64, i64)))
  })
  (T: integral with t = L.terminal) = {
  type terminal = L.terminal

  #[inline]
  def encode_terminal ((t, (i, j)): (terminal, (i64, i64))) : [24]u8 =
    sized 24 (encode_u64 (T.to_i64 t |> u64.i64)
              ++ encode_u64 (u64.i64 i)
              ++ encode_u64 (u64.i64 j))

  #[inline]
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
    let (a, _, size) =
      loop (result, inputs, size) = copy (num, drop 8 bytes, length num)
      for _i < u64.to_i64 num_tests do
        let input_size = u64.to_i64 (decode_u64 (take 8 inputs))
        let inputs' = drop 8 inputs
        let input = take input_size inputs'
        let inputs'' = drop input_size inputs'
        let output = L.lex chunk_size input |> encode_terminals
        let new_size = size + length output
        let result =
          if length result <= new_size
          then scatter (replicate (2 * new_size) 0) (indices result) result
          else result
        let result = scatter result (map (+ size) (indices output)) output
        in (result, inputs'', new_size)
    in take size a
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

  def encode_productions [n] (ts: opt ([n]production)) : []u8 =
    match ts
    case #some ts' ->
      [u8.bool true]
      ++ encode_u64 (u64.i64 n)
      ++ flatten (map (encode_u64 <-< u64.i64 <-< Q.to_i64) ts')
    case #none -> [u8.bool false]

  def test [n] (bytes: [n]u8) : []u8 =
    let num = take 8 bytes
    let num_tests = decode_u64 num
    let (a, _, size) =
      loop (result, inputs, size) = copy (num, drop 8 bytes, length num)
      for _i < u64.to_i64 num_tests do
        let input_size = u64.to_i64 (decode_u64 (take 8 inputs))
        let inputs' = drop 8 inputs
        let input =
          take (input_size * 8) inputs'
          |> unflatten
          |> map (T.u64 <-< decode_u64)
        let inputs'' = drop (input_size * 8) inputs'
        let output =
          P.pre_productions input
          |> encode_productions
        let new_size = size + length output
        let result =
          if length result <= new_size
          then scatter (replicate (2 * new_size) 0) (indices result) result
          else result
        let result = scatter result (map (+ size) (indices output)) output
        in (result, inputs'', new_size)
    in take size a
}

module lexer_parser_test
  (P: {
    type terminal
    type production
    type node 't 'p = #terminal t (i64, i64) | #production p
    val parse [n] : [n]u8 -> opt ([](i64, node terminal production))
  })
  (T: integral with t = P.terminal)
  (Q: integral with t = P.production) = {
  type terminal = P.terminal
  type production = P.production
  type node 't 'p = P.node t p

  def encode_node (p: i64) (n: node terminal production) : []u8 =
    match n
    case #production t ->
      [0u8]
      ++ ((encode_u64 <-< u64.i64) p)
      ++ ((encode_u64 <-< u64.i64 <-< Q.to_i64) t)
      ++ encode_u64 0
      ++ encode_u64 0
    case #terminal t (i, j) ->
      [1u8]
      ++ ((encode_u64 <-< u64.i64) p)
      ++ ((encode_u64 <-< u64.i64 <-< T.to_i64) t)
      ++ ((encode_u64 <-< u64.i64) i)
      ++ ((encode_u64 <-< u64.i64) j)

  def encode_tree [n] (ns: opt ([n](i64, P.node terminal production))) : []u8 =
    match ns
    case #some ns' ->
      [u8.bool true]
      ++ encode_u64 (u64.i64 n)
      ++ flatten (map (uncurry encode_node) ns')
    case #none -> [u8.bool false]

  def test [n] (bytes: [n]u8) : []u8 =
    let num = take 8 bytes
    let num_tests = decode_u64 num
    let (a, _, size) =
      loop (result, inputs, size) = copy (num, drop 8 bytes, length num)
      for _i < u64.to_i64 num_tests do
        let input_size = u64.to_i64 (decode_u64 (take 8 inputs))
        let inputs' = drop 8 inputs
        let input = take input_size inputs'
        let inputs'' = drop input_size inputs'
        let output =
          P.parse input
          |> encode_tree
        let new_size = size + length output
        let result =
          if length result <= new_size
          then scatter (replicate (2 * new_size) 0) (indices result) result
          else result
        let result = scatter result (map (+ size) (indices output)) output
        in (result, inputs'', new_size)
    in take size a
}

-- End of test.fut
