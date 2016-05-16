namespace WordPredictor

module Option =
  let withDefault d option =
    match option with 
            | None -> d
            | Some x -> x

  let case option d f =
    match option with 
            | None -> d
            | Some x -> f x

module Seq =
  let unfoldInfinite next init =
    let generator state = Some $ next state
    Seq.unfold generator init

  let randInt max =
    let rand = new System.Random()
    Seq.initInfinite (fun _ -> rand.Next(max))

  let random () =
    let rand = new System.Random()
    Seq.initInfinite (fun _ -> rand.NextDouble())

  let join sep seq = 
    match Seq.tryHead seq with
    | None -> ""
    | Some head -> 
      Seq.fold (fun s1 s2 -> s1 + sep + s2) head $ Seq.tail seq

  let takeGently n (s: seq<'t>) =
    if n = 0 then Seq.empty else  
    seq { 
      let e = s.GetEnumerator() 
      for _ in 0 .. n - 1 do
        if not (e.MoveNext()) then ()
        else yield e.Current 
    }  

module Array =
  let join (sep: string) (arr: string[]) =
    if arr.Length = 0 then "" else
    let sb = System.Text.StringBuilder()
    for i in 0 .. arr.Length - 2 do
      sb.Append(arr.[i]).Append(sep) |> ignore
    sb.Append(arr.[arr.Length - 1]) |> ignore
    sb.ToString()

module Map =
  let keys map = List.map fst $ Map.toList map
  let values map = List.map snd $ Map.toList map

  let update k d f map = 
    let newValue = 
      match Map.tryFind k map with
      | None -> d
      | Some v -> f v
    (Map.add k newValue map, newValue)

  let updateMap k d f map = fst $ update k d f map

module Async =
  let Sequential seq = async {
    let mutable list = []
    for elemAsync in seq do
      let! elem = elemAsync
      list <- elem :: list
    return Array.rev $ Array.ofList list
  }
