namespace WordPredictor

/// Predicts the next word.
type Predictor =
  /// Given the previous word, returns a word probability distribution the next
  /// word. Should be called once for each word in a sequence.
  abstract nextWordDistribution : string -> Async<Map<string, float>>

type StaticPredictor() =
  let mostCommonWords = ["the"; "be"; "to"; "of"; "and"; "a"; "in"; "that";
    "have"; "it"; "for"; "not"; "on"; "with"; "he"; "as"; "."; ","]
  let distribution = 
    let probability = 0.99999 / (float $ List.length mostCommonWords)
    mostCommonWords
    |> List.map (fun word -> (word, probability))
    |> Map.ofList

  interface Predictor with
    member __.nextWordDistribution word = async {
      return distribution
    }