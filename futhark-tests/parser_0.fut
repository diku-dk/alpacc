import "lib/github.com/diku-dk/sorts/radix_sort"

type bracket = #left u64 | #right u64 | #epsilon
type maybe 'a = #just a | #nothing

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

def key_to_config (key : ((u32), (u32))) : maybe ([]bracket, []u32) =
  match key
  case ((0), (1)) -> #just ([#right 6, #right 1], [3])
  case ((0), (2)) -> #just ([#right 6, #epsilon], [4])
  case ((0), (3)) -> #just ([#right 6, #epsilon], [2])
  case ((2), (1)) -> #just ([#right 1, #epsilon], [u32.highest])
  case ((3), (1)) -> #just ([#right 1, #epsilon], [u32.highest])
  case ((4294967295), (0)) -> #just ([#left 1, #left 6], [0])
  case _ -> #nothing

def is_left (b : bracket) : bool =
  match b
  case #left _ -> true
  case #right _ -> false
  case #epsilon -> false

def depths [n] (input : [n]bracket) : maybe ([n]i64) =
  let left_brackets =
    input
    |> map (is_left)
  let bracket_scan =
    left_brackets
    |> map (\b -> if b then 1 else -1)
    |> scan (+) 0
  let result =
    bracket_scan
    |> map2 (\a b -> b - i64.bool a) left_brackets
  in if any (<0) bracket_scan || last bracket_scan != 0
  then #nothing
  else #just result

def grade (xs : []i64) =
  zip xs (indices xs)
  |> radix_sort_int_by_key (.0) i64.num_bits i64.get_bit
  |> map (.1)

def even_indices 'a [n] (_ : [n]a) =
  iota (n / 2) |> map (2*)

def unpack_bracket (b : bracket) : u64 =
  match b
  case #left a -> a
  case #right a -> a
  case #epsilon -> assert false 0

def eq_no_bracket (a : bracket) (b : bracket) : bool =
  unpack_bracket a u64.== unpack_bracket b

def brackets_matches [n] (brackets : [n]bracket) =
  match depths brackets
  case #just depths' ->
    let grade' = grade depths'
    in even_indices grade'
      |> map (\i -> eq_no_bracket brackets[grade'[i]] brackets[grade'[i+1]])
      |> and
  case #nothing -> false

def is_nothing 'a (m : maybe a) : bool =
  match m
  case #just _ -> false
  case #nothing -> true

def from_just 'a (ne : a) (m : maybe a) : a =
  match m
  case #just a -> a
  case _ -> ne

entry parse [n] (arr : [n]u32) : []u32 =
  let arr' = [0] ++ (map (+2) arr) ++ [1]
  let configs = keys arr' |> map key_to_config
  in if any (is_nothing) configs
  then []
  else
  let ne = ([#epsilon, #epsilon], [u32.highest])
  let (brackets, productions) = configs |> map (from_just ne) |> unzip
  in if brackets 
        |> flatten
        |> filter (!=#epsilon)
        |> brackets_matches
  then productions
    |> flatten
    |> filter (!=u32.highest)
    |> tail
    |> map (\a -> a - 1)
  else []
