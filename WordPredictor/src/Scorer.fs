namespace WordPredictor

/// Scores a Predictor on how well it predicts words in a given sequence.
type Scorer() = 

  static member isValidDistribution (distribution: Map<string, float>) =
    let values = Map.values distribution
    Seq.sum values <= 1.0
      && Seq.forall Num.isNonNegative values

  /// Computes a score when given the predicted word probability distribution
  /// and the correct word.
  abstract score: Map<string, float> -> string -> float

  default __.score distribution word = 
    if not $ Scorer.isValidDistribution distribution then 0.0 else
    match Map.tryFind word distribution with
    | None -> printfn "not valid distribution %s" $ distribution.ToString(); 0.0
    | Some p -> p

  member self.scoreSeq (predictor: Predictor) words =
    let getScore word = async {
      let! distribution = predictor.nextWordDistribution word
      return self.score distribution word
    } 
    Seq.map getScore words

  /// Computes the average score for a Predictor across a sequence of words.
  member self.predictorScore predictor words = async {
    let! scores = Async.Sequential $ self.scoreSeq predictor words
    return Array.average scores
  }

type RelaxedScorer(?relaxationConstant: float) =
  inherit Scorer()
  override __.score distribution word = 
    if not $ Scorer.isValidDistribution distribution then 0.0 else
    match Map.tryFind word distribution with
    | None -> 0.0
    | Some p -> 
      let c = Option.withDefault 2.0 relaxationConstant
      1.0 - (1.0 - p) ** c