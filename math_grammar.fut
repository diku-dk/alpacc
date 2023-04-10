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
  case ((0), (2)) -> [1, 4, 7]
  case ((0), (7)) -> [1, 4, 8]
  case ((2), (1)) -> [u32.highest, u32.highest, u32.highest]
  case ((2), (3)) -> [u32.highest, u32.highest, u32.highest]
  case ((2), (4)) -> [u32.highest, u32.highest, u32.highest]
  case ((2), (5)) -> [u32.highest, u32.highest, u32.highest]
  case ((2), (6)) -> [u32.highest, u32.highest, u32.highest]
  case ((2), (8)) -> [u32.highest, u32.highest, u32.highest]
  case ((3), (2)) -> [1, 4, 7]
  case ((3), (7)) -> [1, 4, 8]
  case ((4), (2)) -> [u32.highest, 4, 7]
  case ((4), (7)) -> [u32.highest, 4, 8]
  case ((4294967295), (0)) -> [u32.highest, u32.highest, 0]
  case ((5), (2)) -> [1, 4, 7]
  case ((5), (7)) -> [1, 4, 8]
  case ((6), (2)) -> [u32.highest, 4, 7]
  case ((6), (7)) -> [u32.highest, 4, 8]
  case ((7), (2)) -> [1, 4, 7]
  case ((7), (7)) -> [1, 4, 8]
  case ((8), (1)) -> [u32.highest, u32.highest, u32.highest]
  case ((8), (3)) -> [u32.highest, u32.highest, u32.highest]
  case ((8), (4)) -> [u32.highest, u32.highest, u32.highest]
  case ((8), (5)) -> [u32.highest, u32.highest, u32.highest]
  case ((8), (6)) -> [u32.highest, u32.highest, u32.highest]
  case ((8), (8)) -> [u32.highest, u32.highest, u32.highest]
  case _ -> assert false [u32.highest, u32.highest, u32.highest]

def parse [n] (arr : [n]u32) =
  let arr' = [0] ++ (map (+2) arr) ++ [1]
  in keys arr'
  |> map key_to_productions
  |> flatten
  |> filter (!=u32.highest)
  |> tail
  |> map (\a -> a - 1)

