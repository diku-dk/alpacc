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
  case ((2), (2)) -> [1, 5]
  case ((2), (5)) -> [1, 4]
  case ((3), (1)) -> [u32.highest, 3]
  case ((3), (3)) -> [u32.highest, 3]
  case ((3), (4)) -> [u32.highest, 2]
  case ((4), (2)) -> [u32.highest, 5]
  case ((4), (5)) -> [u32.highest, 4]
  case ((4294967295), (0)) -> [u32.highest, 0]
  case ((4294967295), (2)) -> [1, 5]
  case ((4294967295), (5)) -> [1, 4]
  case ((5), (1)) -> [u32.highest, 3]
  case ((5), (3)) -> [u32.highest, 3]
  case ((5), (4)) -> [u32.highest, 2]
  case _ -> assert false [u32.highest, u32.highest]

def parse [n] (arr : [n]u32) =
  let arr' = [0] ++ (map (+2) arr) ++ [1]
  in keys arr'
  |> map key_to_productions
  |> flatten
  |> filter (!=u32.highest)
  |> tail
  |> map (\a -> a - 1)

