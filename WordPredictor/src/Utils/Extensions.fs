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

  let join seq sep = 
    match Seq.tryHead seq with
    | None -> ""
    | Some head -> 
      Seq.fold (fun s1 s2 -> s1 + sep + s2) head $ Seq.tail seq

module Map =
  let keys map = List.map fst $ Map.toList map
  let values map = List.map snd $ Map.toList map

module Async =
  let Sequential seq = async {
    let mutable list = []
    for elemAsync in seq do
      let! elem = elemAsync
      list <- elem :: list
    return Array.rev $ Array.ofList list
  }
