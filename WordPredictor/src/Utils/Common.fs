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
  let increment x = x + 1
  let decrement x = x - 1
  let incrementF x = x + 1.0
  let decrementF x = x - 1.0
