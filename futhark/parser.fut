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
  type lookahead_type
  type lookback_type
  val number_of_terminals: i64
  val number_of_productions: i64
  val production_to_terminal: [number_of_productions](opt terminal_module.t)
  val production_to_arity: [number_of_productions]i16
  val lookback_array_to_tuple [n]: [n]terminal_module.t -> lookback_type
  val lookahead_array_to_tuple [n]: [n]terminal_module.t -> lookahead_type
  val start_terminal: terminal_module.t
  val end_terminal: terminal_module.t
  val ne: ([max_ao]bracket_module.t, [max_pi]production_module.t)
  val key_to_config: (lookback_type, lookahead_type) -> opt ([max_ao]bracket_module.t, [max_pi]production_module.t)
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
  
  def lookback_chunks [n] (arr: [n]terminal): [n]P.lookback_type =
    let arr' = replicate P.q empty_terminal ++ arr
    in iota n |> map (\i -> arr'[i:i + P.q] |> P.lookback_array_to_tuple)

  def lookahead_chunks [n] (arr: [n]terminal): [n]P.lookahead_type =
    let arr' = arr ++ replicate P.k empty_terminal
    in iota n |> map (\i -> arr'[i:i + P.k] |> P.lookahead_array_to_tuple)

  def keys [n] (arr: [n]terminal): [n](P.lookback_type, P.lookahead_type) =
    let lookback = lookback_chunks arr
    let lookahead = lookahead_chunks arr
    in zip lookback lookahead

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

  def pre_productions [n] (arr: [n]terminal): []production =
    let arr' = [P.start_terminal] ++ arr ++ [P.end_terminal]
    let configs = keys arr' |> map P.key_to_config
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
    
  -- https://futhark-lang.org/examples/binary-search.html 
  def binary_search [n] 't (lte: t -> t -> bool) (xs: [n]t) (x: t) : i64 =
  let (l, _) =
    loop (l, r) = (0, n-1) while l < r do
    let t = l + (r - l) / 2
    in if x `lte` xs[t]
       then (l, t)
       else (t+1, r)
  in l

  def lte (a0: i64, b0: i16)
          (a1: i64, b1: i16) :
          bool =
    (b0 == b1 && a0 <= a1) || b0 < b1

  def parents [n] (ps: [n]production): [n]i64 =
    let num_bits = i64.num_bits + i16.num_bits
    let get_bit idx (i, d) =
      let l = i32.bool (idx >= i64.num_bits)
      let gte = i32.bool (idx < i64.num_bits)
      in (i16.get_bit (idx - i64.num_bits) d) * l +
         (i64.get_bit idx i) * gte
    let idxs =
      #[trace]
      ps
      |> map (
           \p ->
             let arity = production_to_arity p
             in arity - 1
         )
      |> exscan (+) 0
      |> zip (iota n)
    let sorted_idxs =
      #[trace] blocked_radix_sort 256 num_bits get_bit idxs
    in #[trace] -- This just does not work.
       map (
         \(i, d) ->
           binary_search lte sorted_idxs (i - 1, d)
           |> (\j -> sorted_idxs[i64.max 0 (j - 1)].0)
       ) idxs

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
