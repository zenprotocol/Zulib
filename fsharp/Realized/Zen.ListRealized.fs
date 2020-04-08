module Zen.ListRealized

open Zen.Base
open Zen.Cost
open Prims

module Cost = Zen.Cost.Realized

let rec private revAux (ls : 'a Prims.list) (acc : 'a Prims.list) : 'a Prims.list =
  match ls with
  | Nil' -> acc
  | Cons' (_, hd, tl) -> revAux tl (Cons (hd, acc))

let reverse (ls : 'a Prims.list) =
  lazy (revAux ls Nil')
  |> Cost.C
