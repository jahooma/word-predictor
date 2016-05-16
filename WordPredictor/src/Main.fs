namespace WordPredictor

module Main =

  let wordsFile = "./scraped_wikipedia_articles.txt"

  let scrapeWikipedia numArticles =
    // Download text of random articles
    let wikipediaScraper = WikipediaScraper()
    let words = wikipediaScraper.WordsFromRandomArticles(numArticles)
                |> Async.RunSynchronously 
    let wikiText = Seq.join " " words
    printStr wikiText
    System.IO.File.WriteAllText(wordsFile, wikiText)
    words

  let loadWordsFromFile() =
    let words = System.IO.File.ReadAllText(wordsFile)
    words.Split(' ')

  [<EntryPoint>]
  let main argv = 
    let words = scrapeWikipedia 20
    //let words = loadWordsFromFile()
    let predictor = CombinedNGramPredictor(3)
    let scorer = RelaxedScorer()
    let finalScore = scorer.predictorScore predictor words 
                     |> Async.RunSynchronously 
    printfn "\nFinal Score: %s" $ finalScore.ToString()

    printfn "Press any key to exit."
    System.Console.ReadKey() |> ignore
    0
