namespace WordPredictor

module DataStructures =
  module ApproxDoubleSortedMap =
    type t<'k when 'k : comparison> = Map<'k, float> * Map<float, 'k> * (unit -> float)
    
    let emptyWithVariance x : t<'k> = 
      let randGen = System.Random()
      let rand = fun () -> x * randGen.NextDouble()
      (Map.empty, Map.empty, rand)

    let empty() = emptyWithVariance 0.001

    let add k v (map1, map2, rand) : t<'k> =
      let v' = v + rand()
      (Map.add k v' map1, Map.add v' k map2, rand)

    let remove k ((map1, map2, rand) as state) : t<'k> =
      match Map.tryFind k map1 with
      | None -> state
      | Some v -> (Map.remove k map1, Map.remove v map2, rand)

    let update k f d ((map1, map2, rand) as state) : t<'k> =
      match Map.tryFind k map1 with
      | None -> add k d state
      | Some v -> remove k state |> add k (f v)

    let toKeysOrderSeq (map1, _, _) = Map.toSeq map1
    
    let toValuesOrderSeq (_, map2, _) = 
      Map.toSeq map2 
      |> Seq.map (fun (v, k) -> k, v)
  