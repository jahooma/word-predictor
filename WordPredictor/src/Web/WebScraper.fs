namespace WordPredictor

open System.Threading
open FSharp.Data

module Web = 

  type private AutoReleaseSemaphore(n : int) =
    let semaphore = new Semaphore (initialCount = n, maximumCount = n)
    member x.Acquire() =
      if semaphore.WaitOne() then 
        { new System.IDisposable with
          member x.Dispose() = semaphore.Release() |> ignore }
      else failwith "couldn't acquire a semaphore"

  type WebScraper(?simultaneousRequests: int) =
    // Restrict the number of active web requests with semaphore
    let requestSemaphore = AutoReleaseSemaphore $
                             Option.withDefault 5 simultaneousRequests

    member self.FetchHtml(url: string) = async {
      try
        use holder = requestSemaphore.Acquire()
        return! HtmlDocument.AsyncLoad url
      with :? System.Net.WebException as ex ->
        printStr ex.Message 
        do! Async.Sleep 1000
        return! self.FetchHtml(url)
    }
