namespace WordPredictor

module NGram =
  type t<'a, 'b when 'b : comparison> = {
    prev: 'a list;
    concat: 'a list -> 'b;
    map: Map<'b, int>;
  }

  let empty n concat startToken = {
    prev = List.replicate n startToken;
    concat = concat;
    map = Map.empty;
  }

  let private add token prev = 
    token :: prev
    |> List.take (List.length prev) 

  let insertToken token state =
    let prev' = add token state.prev
    let combined = state.concat prev'
    let map', count = state.map |> Map.update combined 1 Num.increment
    { state with map = map'; prev = prev' }

  let lookup tokens state = 
    state.map
    |> Map.tryFind (state.concat tokens)
    |> Option.withDefault 0

  let lookupNext token state = lookup (add token state.prev) state


module HashTableNGram =
  open System.Collections.Generic

  type t<'a, 'b when 'b : comparison> = {
    prev: 'a[];
    mutable i: int;
    concat: 'a[] -> 'b;
    map: Dictionary<'b, int>;
  }

  let empty n concat startToken = {
    prev = Array.replicate n startToken;
    i = 0;
    concat = concat;
    map = Dictionary();
  }

  let private add token state = 
    state.prev.[state.i] <- token
    state.i <- (state.i + 1) % state.prev.Length

  let insertToken token state =
    add token state
    let combined = state.concat state.prev
    let b, v = state.map.TryGetValue combined
    let v' = if b then v + 1 else 1
    state.map.[combined] <- v'

  let lookup tokens state = 
    let combined = state.concat tokens
    let b, v = state.map.TryGetValue combined
    if b then v else 0

  let lookupNext token state = 
    let temp = state.prev.[state.i]
    state.prev.[state.i] <- token
    let v = lookup state.prev state
    state.prev.[state.i] <- temp
    v
