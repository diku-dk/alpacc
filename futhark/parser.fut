-- Start of parser.fut
--
-- The generic LLP parsing machine, expressed as a parameterised
-- module.

import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/sorts/merge_sort"
import "lib/github.com/diku-dk/containers/opt"

module type parser_context = {
  module terminal_module: integral
  module production_module: integral
  module bracket_module: integral
  val empty_terminal: terminal_module.t
  val empty_production: production_module.t
  val epsilon: bracket_module.t
  val q: i64
  val k: i64
  val max_ao: i64
  val max_pi: i64
  type look_type
  val hash_table_size: i64
  val hash_table_mem_size: i64
  val number_of_terminals: i64
  val number_of_productions: i64
  val production_to_terminal: [number_of_productions](opt terminal_module.t)
  val production_to_arity: [number_of_productions]i16
  val array_to_look_type [n]: [n]terminal_module.t -> look_type
  val start_terminal: terminal_module.t
  val end_terminal: terminal_module.t
  val ne: ([max_ao]bracket_module.t, [max_pi]production_module.t)
  val hash_table: [hash_table_mem_size](opt (look_type, ([max_ao]bracket_module.t, [max_pi]production_module.t)))
  val offset_array: [hash_table_size]i64
  val size_array: [hash_table_size]i64
  val consts_array: [hash_table_size](opt look_type)
  val consts: look_type
  val hash_no_mod: look_type -> look_type -> terminal_module.t
  val look_eq: look_type -> look_type -> bool
}

module mk_parser(P: parser_context) = {
  module terminal_module = P.terminal_module
  module production_module = P.production_module
  module bracket_module = P.bracket_module
                                      
  type terminal = terminal_module.t
  type production = production_module.t
  type bracket = bracket_module.t
  
  def empty_terminal = P.empty_terminal
  def empty_production = P.empty_production
  def epsilon = P.epsilon
                                      
  def is_left (s: bracket): bool =
    bracket_module.get_bit (bracket_module.num_bits - 1) s
    |> bool.i32
  
  def keys [n] (arr: [n]terminal): [n]P.look_type =
    let arr' =
      (replicate P.q empty_terminal)
      ++ arr
      ++ (replicate P.k empty_terminal)
    in iota (n)
       |> map (
            \i ->
              arr'[i:i + P.q + P.k]
              |> P.array_to_look_type
          )

  def depths [n] (input: [n]bracket): opt ([n]i64) =
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
       then #none
       else #some result

  def grade [n] (xs: [n]i64): [n]i64 =
    zip xs (indices xs)
    |> blocked_radix_sort_int_by_key 256 (.0) i64.num_bits i64.get_bit
    |> map (.1)

  def even_indices 'a [n] (_: [n]a): [n / 2]i64 =
    iota (n / 2) |> map (2*)

  def unpack_bracket (b: bracket): bracket =
    bracket_module.set_bit (bracket_module.num_bits - 1) b 0

  def eq_no_bracket (a: bracket) (b: bracket): bool =
    unpack_bracket a bracket_module.== unpack_bracket b

  def brackets_matches [n] (brackets: [n]bracket): bool =
    match depths brackets
    case #some depths' ->
      let grade' = grade depths'
      in even_indices grade'
         |> map (\i -> eq_no_bracket brackets[grade'[i]] brackets[grade'[i+1]])
         |> and
    case #none -> false

  def hash size a b =
    terminal_module.to_i64 (P.hash_no_mod a b) % size
      
  def key_to_config (key: P.look_type): opt ([P.max_ao]bracket_module.t, [P.max_pi]production_module.t) =
    let seg_idx = hash P.hash_table_size P.consts key
    let seg_off = P.offset_array[seg_idx]
    let size = P.size_array[seg_idx]
    in copy P.consts_array[seg_idx]
       |> map_opt (hash size key)
       |> bind_opt (\i -> copy P.hash_table[(seg_off + i) % P.hash_table_mem_size])
       |> bind_opt (
            \(key', value) ->
              if key `P.look_eq` key' then #some value else #none
          )

  def pre_productions [n] (arr: [n]terminal): []production =
    let arr' = [P.start_terminal] ++ arr ++ [P.end_terminal]
    let configs = keys arr' |> map key_to_config
    in if any (is_none) configs
       then []
       else
       let (brackets, productions) = unzip (map (from_opt P.ne) configs)
       in if brackets |> flatten |> filter (bracket_module.!=epsilon) |> brackets_matches
          then productions
               |> flatten
               |> filter (production_module.!=empty_production)
          else []
       
  def production_to_terminal (p: production): opt terminal =
    copy P.production_to_terminal[production_module.to_i64 p]

  def production_to_arity (p: production): i16 =
    copy P.production_to_arity[production_module.to_i64 p]
         
  def productions [n] (arr: [n]terminal): []production =
    let r =
      pre_productions arr
      |> filter (is_none <-< production_to_terminal)
    in if length r == 0
       then []
       else tail r
            |> map (
                 \a -> a production_module.- (production_module.i64 1)
               )

  def exscan [n] 't (op: t -> t -> t) (ne: t) (arr: [n]t): [n]t =
    if n == 0
    then arr
    else let sc = scan op ne arr |> rotate (-1)
         let sc[0] = ne
         in sc

  def size (h: i64): i64 =
    (1 << h) - 1
    
  def mk_tree [n] 't (op: t -> t -> t) (ne: t) (arr: [n]t) =
    let temp = i64.num_bits - i64.clz n
    let h = i64.i32 <| if i64.popc n == 1 then temp else temp + 1
    let tree_size = size h
    let offset = size (h - 1)
    let offsets = iota n |> map (+offset)
    let tree = scatter (replicate tree_size ne) offsets arr
    let arr = copy tree[offset:]
    let (tree, _, _) =
      loop (tree, arr, level) = (tree, arr, h - 2) while level >= 0 do
      let new_size = length arr / 2
      let new_arr =
        tabulate new_size (
                   \i -> arr[2 * i] `op` arr[2 * i + 1]
                 )
      let offset = size level
      let offsets = iota new_size |> map (+offset)
      let new_tree = scatter tree offsets new_arr
      in (new_tree, new_arr, level - 1)
    in tree

  def find_previous [n] 't
                    (op: t -> t -> bool)
                    (tree: [n]t)
                    (idx: i64): i64 =
    let sibling i = i - i64.bool (i % 2 == 0) + i64.bool (i % 2 == 1)
    let parent i = (i - 1) / 2
    let is_left i = i % 2 == 1
    let h = i64.i32 <| i64.num_bits - i64.clz n
    let offset = size (h - 1)
    let start = offset + idx
    let v = tree[start]
    let ascent i = i != 0 && (is_left i || !(tree[sibling i] `op` v))
    let descent i = 2 * i + 1 + i64.bool (tree[2 * i + 2] `op` v)
    let index = iterate_while ascent parent start
    in if index != 0
       then iterate_while (< offset) descent (sibling index) - offset
       else -1

  def parents [n] (ps: [n]production): [n]i64 =
    let tree =
      map production_to_arity ps
      |> map ((+ -1) <-< i64.i16)
      |> exscan (+) 0
      |> mk_tree i64.min i64.highest
    let parents' = map (find_previous (<=) tree) (iota n)
    in if n == 0
       then parents'
       else let parents'[0] = 0 in parents'

  def backwards_linear_search [n] 't
                              (op: t -> t -> bool)
                              (arr: [n]t)
                              (i: i64): i64 =
    loop j = i - 1 while j != -1 && not (arr[j] `op` arr[i]) do
      j - 1

  def test_previous_equal_or_smaller [n] (arr: [n]i32): bool =
    let expected = map (backwards_linear_search (<=) arr) (iota n)
    let tree = mk_tree i32.min i32.highest arr
    let result = map (find_previous (<=) tree) (iota n)
    in zip expected result
       |> all (uncurry (==))
            
  type node 't 'p = #terminal t (i32, i32) | #production p
  
  def terminal_offsets [n][m]
                       (spans: [m](i32, i32))
                       (ts: [n](opt terminal)):
                       [m](i64, node terminal production) =
    map (is_some) ts
    |> zip3 (iota n) (ts)
    |> filter (\(_, _, b) -> b)
    |> sized m
    |> zip spans
    |> map (
         \(s, (i, t, _)) ->
           from_opt empty_terminal t
           |> (\t' -> (i, #terminal t' s))
       )
    
  def parse [n] (arr: [n](terminal, (i32, i32))) =
    let (ters, spans) = unzip arr
    let prods = ters |> pre_productions |> tail
    let parent_vector = parents prods
    let ts = map production_to_terminal prods
    let (offsets, ts') = terminal_offsets spans ts |> unzip
    let prods' =
      map (
        \p -> #production (p production_module.- (production_module.i64 1))
      ) prods :> [](node terminal production)
    in scatter prods' offsets ts'
       |> zip parent_vector
}

-- End of parser.fut
