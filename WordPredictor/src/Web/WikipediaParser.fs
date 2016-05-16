namespace WordPredictor

open System.Text.RegularExpressions
open FSharp.Data

module WikipediaParser =
  let private punctuation = ".,!?;:~`@#$%\^&*\(\)\+=\"\'\{\<>|}‘’"

  let private cleanStr (str: string) =
    let separatePunctuation str = 
      Regex.Replace(str, "[" + punctuation + "]", 
       MatchEvaluator(fun matched -> " " + matched.Value + " "))
    let removeBrackets str = 
      Regex.Replace(str, "\[[^\[\]]*?\]", " ")
    let replaceWhiteSpace str =
      Regex.Replace(str, "\s+", " ")
    str
    |> separatePunctuation
    |> removeBrackets
    |> replaceWhiteSpace

  let private splitIntoWords (str: string) =
    str.Split(' ')
    |> Seq.ofArray
    |> Seq.filter (not << System.String.IsNullOrWhiteSpace)

  let private truncateAtEndOfArticle nodes =
    let hasEndId node =
      let endIds = ["See_also"; "References"; "External_links"]
      Seq.exists (flip HtmlNode.hasId $ node) endIds
    let isNotEndHeader node = 
      node 
      |> HtmlNode.descendants false hasEndId
      |> Seq.isEmpty
    nodes |> Seq.takeWhile isNotEndHeader

  let private chooseContentChildren content = 
    let isNodeIncluded node = 
      let admissibleTags = Set.ofList ["p"; "h1"; "h2"; "h3"]
      let tagName node = (HtmlNode.name node).ToLower()
      Set.contains (tagName node) admissibleTags 
    HtmlNode.elements content 
    |> Seq.filter isNodeIncluded
    |> truncateAtEndOfArticle

  let private getContentNodes htmlDoc =
    let nodeWithId root id = 
      HtmlNode.descendants false (HtmlNode.hasId id) root

    let getTitle body = nodeWithId body "firstHeading" 

    let getContent body = nodeWithId body "mw-content-text"

    let bodyOpt = HtmlDocument.tryGetBody htmlDoc
    Option.case bodyOpt Seq.empty $ fun body ->
      let title = getTitle body
      let content = 
        getContent body 
        |> Seq.map chooseContentChildren
        |> Seq.concat
      Seq.append title content

  let newArticleWord = "<new-article>"

  /// Extract the words from the main content of a wikipedia article.
  let htmlToWordSeq (htmlDoc: HtmlDocument) =
    getContentNodes htmlDoc 
    |> Seq.map HtmlNode.innerText
    |> Seq.fold (fun s1 s2 -> s1 + " " + s2 + " ") ""
    |> cleanStr
    |> (fun str -> newArticleWord + " " + str)
    |> splitIntoWords
