-- Start of parser.fut
--
-- The generic LLP parsing machine, expressed as a parameterised
-- module.

import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/sorts/merge_sort"
import "lib/github.com/diku-dk/containers/opt"
import "lib/github.com/diku-dk/segmented/segmented"

module type parser_context = {
  module terminal_module: integral
  module production_module: integral
  module bracket_module: integral
  val empty_terminal : terminal_module.t
  val q : i64
  val k : i64
  val number_of_productions : i64
  val production_to_terminal : [number_of_productions](opt terminal_module.t)
  val production_to_arity : [number_of_productions]i64
  val start_terminal : terminal_module.t
  val end_terminal : terminal_module.t
  val hash_table_size : i64
  val max_iters : i64
  val productions_size : i64
  val stacks_size : i64
  val hash_table : [hash_table_size](bool, [q + k]terminal_module.t, ((i64, i64), (i64, i64)))
  val stacks : [stacks_size]bracket_module.t
  val productions : [productions_size]production_module.t
}

module mk_parser (P: parser_context) = {
  module terminal_module = P.terminal_module
  module production_module = P.production_module
  module bracket_module = P.bracket_module

  type terminal = terminal_module.t
  type production = production_module.t
  type bracket = bracket_module.t

  def empty_terminal = P.empty_terminal

  def is_left (s: bracket) : bool =
    bracket_module.get_bit (bracket_module.num_bits - 1) s
    |> bool.i32

  def hash [n] (arr: [n]terminal) : u64 =
    foldl (\h a ->
             let h' = h ^ u64.i64 (terminal_module.to_i64 a)
             in h' * 1099511628211)
          14695981039346656037
          arr

  def get_key [n] (arr: [n]terminal) (i: i64) : [P.q + P.k]terminal =
    #[inline]
    #[sequential]
    tabulate (P.q + P.k)
             (\j ->
                if i + j < P.q || i + j >= n + P.q then empty_terminal else arr[i + j - P.q])

  def array_equal [n] 'a (eq: a -> a -> bool) (as: [n]a) (bs: [n]a) : bool =
    #[inline]
    #[sequential]
    map2 eq as bs
    |> and

  def lookup (k: [P.q + P.k]terminal) : ((i64, i64), (i64, i64)) =
    let h = (hash k) %% u64.i64 P.hash_table_size
    let (_, _, _, v) =
      loop (is_found, i, h, v) = (false, 0, h, ((-1, -1), (-1, -1)))
      while is_found || i < P.max_iters do
        let (t, k', v') = P.hash_table[i64.u64 h]
        let is_valid = t && (array_equal (terminal_module.==) k' k)
        in ( is_valid
           , i + 1
           , (h + 1) %% u64.i64 P.hash_table_size
           , if is_valid then v' else v
           )
    in v

  def keys [n] (arr: [n]terminal) : [n]((i64, i64), (i64, i64)) =
    tabulate n
             (\i ->
                let key = get_key arr i
                in lookup key)

  def valid_keys [n] : [n]((i64, i64), (i64, i64)) -> bool =
    all (\((a, b), (c, d)) -> a != -1 && b != -1 && c != -1 && d != -1)

  def depths [n] (input: [n]bracket) : opt ([n]i64) =
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
    in if any (< 0) bracket_scan || (n != 0 && last bracket_scan != 0)
       then #none
       else #some result

  def grade [n] (xs: [n]i64) : [n]i64 =
    zip xs (indices xs)
    |> blocked_radix_sort_int_by_key 256 (.0) i64.num_bits i64.get_bit
    |> map (.1)

  def even_indices 'a [n] (_: [n]a) : [n / 2]i64 =
    iota (n / 2) |> map (2 *)

  def unpack_bracket (b: bracket) : bracket =
    bracket_module.set_bit (bracket_module.num_bits - 1) b 0

  def eq_no_bracket (a: bracket) (b: bracket) : bool =
    unpack_bracket a bracket_module.== unpack_bracket b

  def brackets_matches [n] (brackets: [n]bracket) : bool =
    match depths brackets
    case #some depths' ->
      let grade' = grade depths'
      in even_indices grade'
         |> map (\i -> eq_no_bracket brackets[grade'[i]] brackets[grade'[i + 1]])
         |> and
    case #none -> false

  def gather [n] [m] 'a (arr: [n]a) (is: [m]i64) : [m]a =
    map (\i -> arr[i]) is

  def gather_scatter 'a [n] [m] [k]
                     (dest: *[n]a)
                     (mapping: [m](i64, i64))
                     (vs: [k]a) : *[n]a =
    let (is', is) = unzip mapping
    let vs = gather vs is'
    in scatter dest is vs

  def exscan [n] 'a (op: a -> a -> a) (ne: a) (as: [n]a) =
    if length as == 0
    then (ne, as)
    else let res = scan op ne as |> rotate (-1)
         let l = copy res[0]
         let res[0] = ne
         in (l, res)

  def create_flags [n] 't
                   (default: t)
                   (flags: [n]t)
                   (shape: [n]i64) : []t =
    let (m, offsets) = exscan (+) 0 shape
    let idxs =
      map2 (\i j -> if i == 0 then -1 else j)
           shape
           offsets
    in scatter (replicate m default) idxs flags

  def segmented_copy [n] [m] 'a
                     (arr: [m]a)
                     (spans: [n](i64, i64)) : []a =
    let (starts, ends) = unzip spans
    let shape = map2 (-) ends starts
    let (seg_idxs, idxs) = repl_segm_iota shape
    let dest = replicate (length idxs) arr[0]
    let offsets =
      gather starts seg_idxs
      |> map2 (+) idxs
      |> flip zip (indices idxs)
    in gather_scatter dest offsets arr

  def construct_stacks =
    segmented_copy P.stacks

  def construct_productions =
    segmented_copy P.productions

  def to_keys arr =
    let arr' = [P.start_terminal] ++ arr ++ [P.end_terminal]
    let idxs = keys arr'
    in if valid_keys idxs
       then some idxs
       else #none

  def to_productions [n] (ks: [n]((i64, i64), (i64, i64))) : opt ([]production) =
    let (stack_spans, productions_spans) = unzip ks
    let stacks = construct_stacks stack_spans
    let is_valid = brackets_matches stacks
    let prods =
      if is_valid
      then construct_productions productions_spans
      else []
    in if is_valid
       then #some prods
       else #none

  def pre_productions [n] (arr: [n]terminal) : opt ([]production) =
    let ks' = to_keys arr
    let ks =
      match ks'
      case #some k -> k
      case #none -> []
    let prods = to_productions ks
    in if is_some ks'
       then prods
       else #none

  def production_to_terminal (p: production) : opt terminal =
    copy P.production_to_terminal[production_module.to_i64 p]

  def production_to_arity (p: production) : i64 =
    copy P.production_to_arity[production_module.to_i64 p]

  def size (h: i64) : i64 =
    (1 << h) - 1

  def mk_tree [n] 't (op: t -> t -> t) (ne: t) (arr: [n]t) =
    let temp = i64.num_bits - i64.clz n
    let h = i64.i32 <| if i64.popc n == 1 then temp else temp + 1
    let tree_size = size h
    let offset = size (h - 1)
    let offsets = iota n |> map (+ offset)
    let tree = scatter (replicate tree_size ne) offsets arr
    let arr = copy tree[offset:]
    let (tree, _, _) =
      loop (tree, arr, level) = (tree, arr, h - 2)
      while level >= 0 do
        let new_size = length arr / 2
        let new_arr =
          tabulate new_size (\i -> arr[2 * i] `op` arr[2 * i + 1])
        let offset = size level
        let offsets = iota new_size |> map (+ offset)
        let new_tree = scatter tree offsets new_arr
        in (new_tree, new_arr, level - 1)
    in tree

  def find_previous [n] 't
                    (op: t -> t -> bool)
                    (tree: [n]t)
                    (idx: i64) : i64 =
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

  def parents [n] (ps: [n]production) : [n]i64 =
    let tree =
      map production_to_arity ps
      |> map (+ -1)
      |> exscan (+) 0
      |> (.1)
      |> mk_tree i64.min i64.highest
    let parents' = map (find_previous (<=) tree) (iota n)
    in if n == 0
       then parents'
       else let parents'[0] = 0 in parents'

  def backwards_linear_search [n] 't
                              (op: t -> t -> bool)
                              (arr: [n]t)
                              (i: i64) : i64 =
    loop j = i - 1
    while j != -1 && not (arr[j] `op` arr[i]) do
      j - 1

  def test_previous_equal_or_smaller [n] (arr: [n]i32) : bool =
    let expected = map (backwards_linear_search (<=) arr) (iota n)
    let tree = mk_tree i32.min i32.highest arr
    let result = map (find_previous (<=) tree) (iota n)
    in zip expected result
       |> all (uncurry (==))

  type node 't 'p = #terminal t (i64, i64) | #production p

  def safe_zip [n] [m] 'a 'b (a: [n]a) (b: [m]b) =
    if n == m
    then zip a (sized n b)
    else assert true []

  def terminal_offsets [n] [m]
                       (spans: [m](i64, i64))
                       (ts: [n](opt terminal)) : [](i64, node terminal production) =
    map (is_some) ts
    |> zip3 (iota n) (ts)
    |> filter (\(_, _, b) -> b)
    |> safe_zip spans
    |> map (\(s, (i, t, _)) ->
              from_opt empty_terminal t
              |> (\t' -> (i, #terminal t' s)))

  def parse [n] (arr: [n](terminal, (i64, i64))) : opt ([](i64, node terminal production)) =
    let (ters, spans) = unzip arr
    let prods' = ters |> pre_productions
    let result =
      match prods'
      case #some prods ->
        let parent_vector = parents prods
        let ts = map production_to_terminal prods
        let (offsets, tprods) = terminal_offsets spans ts |> unzip
        let prods =
          map (\p -> #production p)
              prods
          :> [](node terminal production)
        in scatter prods offsets tprods
           |> zip parent_vector
      case _ -> []
    in if is_some prods' then some result else #none
}

-- End of parser.fut
