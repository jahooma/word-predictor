namespace WordPredictor

module Main =

  [<EntryPoint>]
  let main argv = 
    // Download text of 10 random articles
    let wikipediaScraper = WikipediaScraper()
    let numArticles = 10
    let words = Async.RunSynchronously $ wikipediaScraper.WordsFromRandomArticles(numArticles)
    let wikiText = Seq.join words " "
    printStr wikiText
    System.IO.File.WriteAllText("./scraped_wikipedia_articles.txt", wikiText)

    // Run StaticPredictor across words, and score the result
    let predictor = StaticPredictor()
    let scorer = RelaxedScorer()
    let finalScore = Async.RunSynchronously $ scorer.predictorScore predictor words
    printfn "\nFinal Score of StaticPredictor: %s" $ finalScore.ToString()

    printfn "Press any key to exit."
    System.Console.ReadKey() |> ignore
    0
