namespace WordPredictor

module PredictorUtils = 
  let normalizeDistribution distribution =
    let sum = Map.values distribution
              |> List.sum
              |> fun x -> x + 0.001
    distribution
    |> Map.map (fun k v -> v / sum)

  let combineDistributions distribution1 distribution2 scale =
    let scale1 = Option.withDefault 0.5 scale
    let scale2 = 1.0 - scale1
    let scaledDistribution1 = Map.map (fun k t -> scale1 * t) distribution1
    let updateCombined combined k v = Map.updateMap k v (fun v' -> v' + v * scale2) combined
    distribution2
    |> Map.fold updateCombined scaledDistribution1
    |> normalizeDistribution

  type CombinedPredictor(predictor1: Predictor, predictor2: Predictor, ?scale: float) =
    interface Predictor with
      member __.nextWordDistribution word = async {
        let! prediction1 = predictor1.nextWordDistribution word
        let! prediction2 = predictor2.nextWordDistribution word
        return combineDistributions prediction1 prediction2 scale
      }

  type RestartingPredictor(newPredictor: unit -> Predictor, restartCondition: int -> string -> bool) =
    let mutable i = 0
    let mutable predictor = newPredictor()

    interface Predictor with
      member __.nextWordDistribution word = 
        if restartCondition i word then
          predictor <- newPredictor()
        i <- i + 1
        predictor.nextWordDistribution word
    
  type StaticPredictor() =
    let mostCommonWords = ["the"; "be"; "to"; "of"; "and"; "a"; "in"; "that";
      "have"; "it"; "for"; "not"; "on"; "with"; "he"; "as"; "."; ","]
    let distribution = 
      mostCommonWords
      |> List.map (fun word -> word, 1.0)
      |> Map.ofList
      |> normalizeDistribution

    interface Predictor with
      member __.nextWordDistribution word = async {
        return distribution
      }
