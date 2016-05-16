namespace WordPredictor

module DoubleMap = DataStructures.ApproxDoubleSortedMap
module NGram = HashTableNGram

open PredictorUtils

type NGramPredictor(n: int) =
  let numWordsConsidered = 250
  let concat = Array.join "|"
  let mutable nGrams = List.init n $ fun i -> NGram.empty (i + 1) concat ""
  let mutable wordsCount = DoubleMap.empty()
  let mutable i = 0;

  let computeWordScore word =
    nGrams
    |> List.map (fun nGram -> NGram.lookupNext word nGram)
    |> List.map (fun x -> if x <= 1 then 0 else x)
    |> List.sum
    |> float

  let computePredictionDistribution() =
    let addWordScore distribution word = 
      distribution |> Map.add word (computeWordScore word)
    DoubleMap.toValuesOrderSeq wordsCount
    |> Seq.map fst
    |> Seq.takeGently numWordsConsidered
    |> Seq.fold addWordScore Map.empty
    |> PredictorUtils.normalizeDistribution

  interface Predictor with
    member __.nextWordDistribution word = async {
      List.iter (NGram.insertToken word) nGrams
      wordsCount <- DoubleMap.update word Num.decrementF -1.0 wordsCount
      return computePredictionDistribution()
    }

type CombinedNGramPredictor(n: int) =
  let newPredictor = fun () -> NGramPredictor(n) :> Predictor
  let isNewArticle i word = word = WikipediaParser.newArticleWord
  let articlePredictor = RestartingPredictor(newPredictor, isNewArticle)

  let normalPredictor = NGramPredictor(n)
  let combinedPredictor =
    CombinedPredictor(normalPredictor, articlePredictor, 0.6)
    :> Predictor
  
  interface Predictor with
    member __.nextWordDistribution word = async {
      let! distribution = combinedPredictor.nextWordDistribution word
      return distribution
      |> Map.map (fun word score -> score ** 2.0)
      |> normalizeDistribution
    }
