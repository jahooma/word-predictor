namespace WordPredictor

type WikipediaScraper() =
  let randomPageUrl = "https://en.wikipedia.org/wiki/Special:Random"
  let webScraper = Web.WebScraper();

  member __.RandomArticle() = async {
    let! html = webScraper.FetchHtml randomPageUrl
    return WikipediaParser.htmlToWordSeq html
  }

  member self.RandomArticles() = 
    Seq.initInfinite $ fun _ -> self.RandomArticle()

  member self.WordsFromRandomArticles numArticles = async {
    let articlesAsync = Seq.take numArticles $ self.RandomArticles()
    let! articles = Async.Parallel articlesAsync
    return Array.collect Array.ofSeq articles
  }
