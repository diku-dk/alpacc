def chunks (n : i64) (size : i64) (arr : []i64) =
    iota n |> map (\i -> copy arr[i:i + size] :> [size]i64)

def lookahead_chunks [n] (size : i64) (arr : [n]i64) =
    let arr' = arr ++ replicate size 0
    in chunks n size arr'

def lookback_chunks [n] (size : i64) (arr : [n]i64) =
    let arr' = replicate size 0 ++ arr
    in chunks n size arr'

def keys [n] (q : i64) (k : i64) (arr : [n]i64) =
    let lookback = lookback_chunks q arr
    let lookahead = lookahead_chunks k arr
    in zip lookback lookahead