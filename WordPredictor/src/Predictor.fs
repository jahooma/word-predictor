namespace WordPredictor

/// Predicts the next word.
type Predictor =
  /// Given the previous word, returns a word probability distribution the next
  /// word. Should be called once for each word in a sequence.
  abstract nextWordDistribution : string -> Async<Map<string, float>>
