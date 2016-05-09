namespace WordPredictor

[<AutoOpen>]
module Common =
  let ($) x y = x y
  
  let flip f x y = f y x

  let printStr s = printfn "%s" s

module Num =
  let isNonNegative x = x >= 0.0
  let isNegative x = x < 0.0
  let isPositive x = x > 0.0