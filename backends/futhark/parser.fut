-- Start of parser.fut
--
-- The generic LLP parsing machine, expressed as a parameterised
-- module.

import "lib/github.com/diku-dk/containers/core/opt"
import "lib/github.com/diku-dk/segmented/segmented"

module type parser_context = {
  type terminal
  type production
  module terminal_int_module: integral
  module production_int_module: integral
  module bracket_module: integral
  val empty_terminal : terminal_int_module.t
  val q : i64
  val k : i64
  val number_of_productions : i64
  val production_to_terminal : [number_of_productions](opt terminal_int_module.t)
  val production_to_arity : [number_of_productions]u8
  val start_terminal : terminal_int_module.t
  val end_terminal : terminal_int_module.t
  val hash_table_size : i64
  val max_iters : i64
  val productions_size : i64
  val stacks_size : i64
  val hash_table : [hash_table_size](bool, [q + k]terminal_int_module.t, ((i32, i32), (i32, i32)))
  val stacks : [stacks_size]bracket_module.t
  val productions : [productions_size]production_int_module.t
  val production_int_to_name : [number_of_productions]production
  val number_of_terminals : i64
  val terminal_int_to_name : [number_of_terminals]terminal
}

module type parser = {
  type terminal_int
  type terminal
  type production_int
  type production
  -- | The compact parse tree: per production node its id and parent
  -- index, plus per input terminal the index of its parent node in
  -- the tree.
  val parse_int [n] : [n]terminal_int -> opt ([](production_int, idx.t), [n]idx.t)
  val parse [n] : [n]terminal_int -> opt ([](production, idx.t), [n]idx.t)
  val pre_productions_int [n] : [n]terminal_int -> opt ([]production_int)
  val pre_productions [n] : [n]terminal_int -> opt ([]production)
}

module mk_parser (P: parser_context)
  : parser
    with terminal_int = P.terminal_int_module.t
    with terminal = P.terminal
    with production_int = P.production_int_module.t
    with production = P.production = {
  module terminal_int_module = P.terminal_int_module
  module production_int_module = P.production_int_module
  module bracket_module = P.bracket_module

  type terminal_int = terminal_int_module.t
  type terminal = P.terminal
  type production_int = production_int_module.t
  type production = P.production
  type bracket = bracket_module.t

  def empty_terminal : terminal_int = P.empty_terminal

  def is_left (s: bracket) : bool =
    bracket_module.get_bit (bracket_module.num_bits - 1) s
    |> bool.i32

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
                    (pos: i64) : i64 =
    let sibling i = i - i64.bool (i % 2 == 0) + i64.bool (i % 2 == 1)
    let parent i = (i - 1) / 2
    let is_left i = i % 2 == 1
    let h = i64.i32 <| i64.num_bits - i64.clz n
    let offset = size (h - 1)
    let start = offset + pos
    let v = tree[start]
    let ascent i = i != 0 && (is_left i || !(tree[sibling i] `op` v))
    let descent i = 2 * i + 1 + i64.bool (tree[2 * i + 2] `op` v)
    let index = iterate_while ascent parent start
    in if index != 0
       then iterate_while (< offset) descent (sibling index) - offset
       else -1

  def hash [n] (arr: [n]terminal_int) : u64 =
    foldl (\h a ->
             let h' = h ^ u64.i64 (terminal_int_module.to_i64 a)
             in h' * 1099511628211)
          14695981039346656037
          arr

  def get_key [n] (arr: [n]terminal_int) (i: i64) : [P.q + P.k]terminal_int =
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

  def lookup (k: [P.q + P.k]terminal_int) : ((i32, i32), (i32, i32)) =
    let h = (hash k) %% u64.i64 P.hash_table_size
    let (_, _, _, v) =
      loop (is_found, i, h, v) = (false, 0, h, ((-1i32, -1i32), (-1i32, -1i32)))
      while is_found || i < P.max_iters do
        let (t, k', v') = P.hash_table[i64.u64 h]
        let is_valid = t && (array_equal (terminal_int_module.==) k' k)
        in ( is_valid
           , i + 1
           , (h + 1) %% u64.i64 P.hash_table_size
           , if is_valid then v' else v
           )
    in v

  def keys [n] (arr: [n]terminal_int) : [n]((i32, i32), (i32, i32)) =
    tabulate n
             (\i ->
                let key = get_key arr i
                in lookup key)

  def valid_keys [n] : [n]((i32, i32), (i32, i32)) -> bool =
    all (\((a, b), (c, d)) -> a != -1 && b != -1 && c != -1 && d != -1)

  def depths [n] (input: [n]bracket) : opt ([n]idx.t) =
    let left_brackets =
      input
      |> map (is_left)
    let bracket_scan =
      left_brackets
      |> map (\b -> if b then 1i64 else -1i64)
      |> scan (+) 0i64
    let result =
      bracket_scan
      |> map2 (\a b -> idx.i64 (b - i64.bool a)) left_brackets
    in if any (< 0) bracket_scan || (n != 0 && last bracket_scan != 0)
       then #none
       else #some result

  def unpack_bracket (b: bracket) : bracket =
    bracket_module.set_bit (bracket_module.num_bits - 1) b 0

  def eq_no_bracket (a: bracket) (b: bracket) : bool =
    unpack_bracket a bracket_module.== unpack_bracket b

  def brackets_matches [n] (brackets: [n]bracket) : bool =
    match depths brackets
    case #some ds ->
      let tree = mk_tree idx.min idx.highest ds
      in tabulate n (\i ->
                       if is_left brackets[i]
                       then true
                       else let j = find_previous (<=) tree i
                            in eq_no_bracket brackets[i] brackets[j])
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

  def to_keys arr : opt ([]((i32, i32), (i32, i32))) =
    let arr' = [P.start_terminal] ++ arr ++ [P.end_terminal]
    let idxs = keys arr'
    in if valid_keys idxs
       then #some idxs
       else #none

  def widen_span (s: i32, e: i32) : (i64, i64) = (i64.i32 s, i64.i32 e)

  def to_productions [n] (ks: [n]((i32, i32), (i32, i32))) : opt ([]production_int) =
    let (stack_spans, productions_spans) = unzip ks
    let stacks = construct_stacks (map widen_span stack_spans)
    let is_valid = brackets_matches stacks
    let prods =
      if is_valid
      then construct_productions (map widen_span productions_spans)
      else []
    in if is_valid
       then #some prods
       else #none

  def pre_productions_int_flag [n] (arr: [n]terminal_int) : (bool, []production_int) =
    let ks' = to_keys arr
    let is_valid = opt.is_some ks'
    let ks =
      match ks'
      case #some k -> k
      case #none -> []
    let prods = to_productions ks
    let is_valid = is_valid && opt.is_some prods
    let result =
      match prods
      case #some ps -> ps
      case #none -> []
    in (is_valid, result)

  def pre_productions_int [n] (arr: [n]terminal_int) : opt ([]production_int) =
    let (is_valid, prods) = pre_productions_int_flag arr
    in if is_valid then #some prods else #none

  def pre_productions [n] (arr: [n]terminal_int) : opt ([]production) =
    let (is_valid, prods) = pre_productions_int_flag arr
    in if is_valid
       then #some (map (\p -> copy P.production_int_to_name[production_int_module.to_i64 p]) prods)
       else #none

  def production_to_terminal (p: production_int) : opt terminal_int =
    copy P.production_to_terminal[production_int_module.to_i64 p]

  def production_to_arity (p: production_int) : idx.t =
    idx.i64 (i64.u8 (copy P.production_to_arity[production_int_module.to_i64 p]))

  def parents [n] (ps: [n]production_int) : [n]idx.t =
    let tree =
      map production_to_arity ps
      |> map (+ idx.i64 (-1))
      |> exscan (+) (idx.i64 0)
      |> (.1)
      |> mk_tree idx.min idx.highest
    let parents' = map (idx.i64 <-< find_previous (<=) tree) (iota n)
    in if n == 0
       then parents'
       else let parents'[0] = idx.i64 0 in parents'

  -- | Parse a token sequence into the compact tree: the preorder
  -- production list with terminal-production slots dropped, each
  -- remaining node paired with its parent index in the compacted
  -- numbering, plus per input token the parent index of its
  -- (dropped) terminal slot.  Parents always point at production
  -- nodes, so remapping is a subtraction of the number of terminal
  -- slots at or before the old index.
  def parse_int_flag [n] (ters: [n]terminal_int) : (bool, [](production_int, idx.t), [n]idx.t) =
    let prods' = pre_productions_int ters
    let prods =
      match prods'
      case #some ps -> ps
      case #none -> []
    let m = length prods
    let parent_vector = parents prods
    let is_term = map (opt.is_some <-< production_to_terminal) prods
    let t_incl = map i64.bool is_term |> scan (+) 0
    let num_terms = if m == 0 then 0 else t_incl[m - 1]
    let is_valid = opt.is_some prods' && num_terms == n
    let remap p = let p' = idx.to_i64 p in idx.i64 (p' - t_incl[p'])
    let tree =
      zip3 prods parent_vector is_term
      |> filter (\(_, _, b) -> !b)
      |> map (\(p, par, _) -> (p, remap par))
    let token_parents =
      if is_valid
      then zip is_term parent_vector
           |> filter (.0)
           |> map (remap <-< (.1))
           |> sized n
      else replicate n (idx.i64 0)
    in (is_valid, tree, token_parents)

  def parse_int [n] (ters: [n]terminal_int) : opt ([](production_int, idx.t), [n]idx.t) =
    let (is_valid, tree, token_parents) = parse_int_flag ters
    in if is_valid then #some (tree, token_parents) else #none

  def parse [n] (ters: [n]terminal_int) : opt ([](production, idx.t), [n]idx.t) =
    let (is_valid, tree, token_parents) = parse_int_flag ters
    let named =
      map (\(p, par) ->
             (copy P.production_int_to_name[production_int_module.to_i64 p], par))
          tree
    in if is_valid then #some (named, token_parents) else #none
}

-- End of parser.fut
