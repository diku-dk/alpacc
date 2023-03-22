def lookbkack_array_to_tuple [n] (arr : [n]u32) =
  (arr[0])

def lookahead_array_to_tuple [n] (arr : [n]u32) =
  (arr[0])

def lookback_chunks [n] (arr : [n]u32) =
  let arr' = replicate 1 u32.highest ++ arr
  in iota n |> map (\i -> arr'[i:i + 1] |> lookbkack_array_to_tuple)

def lookahead_chunks [n] (arr : [n]u32) =
  let arr' = arr ++ replicate 1 u32.highest
  in iota n |> map (\i -> arr'[i:i + 1] |> lookahead_array_to_tuple)

def keys [n] (arr : [n]u32) =
  let lookback = lookback_chunks arr
  let lookahead = lookahead_chunks arr
  in zip lookback lookahead

def key_to_productions (key : ((u32), (u32))) =
  match key
  case ((0), (2)) -> [u32.highest, 1, 3]
  case ((0), (3)) -> [1, 2, 5]
  case ((0), (5)) -> [1, 2, 4]
  case ((2), (2)) -> [u32.highest, u32.highest, 3]
  case ((2), (3)) -> [u32.highest, 2, 5]
  case ((2), (4)) -> [u32.highest, 2, 4]
  case ((3), (3)) -> [u32.highest, u32.highest, 5]
  case ((3), (4)) -> [u32.highest, u32.highest, 4]
  case ((3), (5)) -> [u32.highest, u32.highest, 4]
  case ((4), (4)) -> [u32.highest, u32.highest, u32.highest]
  case ((4), (5)) -> [u32.highest, u32.highest, u32.highest]
  case ((4294967295), (0)) -> [u32.highest, u32.highest, 0]
  case ((5), (1)) -> [u32.highest, u32.highest, u32.highest]
  case _ -> [u32.highest, u32.highest, u32.highest]

def parse [n] (arr : [n]u32) =
  let arr' = [0] ++ arr ++ [1]
  in keys arr'
  |> map key_to_productions
  |> flatten
  |> filter (!=u32.highest)