module Zen.List.Bounded

open Zen.Base
open Zen.Cost
module OT = Zen.OptionT

module ZL = Zen.List

type t : Type -> Type = list

(* Basic Functions *)


val append(#a:Type):
    maxLength: nat
    -> l1:list a { length l1 <= maxLength }
    -> list a
    -> list a `cost` (maxLength * 2 + 2)
let rec append #_ maxLength l1 l2 =
    ZL.append l1 l2
    |> inc ((maxLength - length l1) * 2)

(* List Transformations *)
val mapT(#a #b:Type)(#n:nat):
  maxLength: nat
  -> (a -> b `cost` n)
  -> ls: list a { length ls <= maxLength }
  -> list b `cost` (maxLength * (n + 2) + 2)
let rec mapT #a #b #n maxLength f ls =
    ZL.mapT f ls
    |> inc ((maxLength - length ls) * (n + 2))

val map(#a #b:Type):
    maxLength: nat
    -> (a -> b)
    -> ls:list a { length ls <= maxLength }
    -> list b `cost` (maxLength * 2 + 2)
let map #_ #_ maxLength f ls =
    ZL.map f ls
    |> inc ((maxLength - length ls) * 2)

val revAppend(#a:Type):
    maxLength: nat
    -> l1:list a { length l1 <= maxLength }
    -> list a
    -> list a `cost` (maxLength * 2 + 2)
let rec revAppend #_ maxLength l1 l2 =
    ZL.revAppend l1 l2
    |> inc ((maxLength - length l1) * 2)

val rev(#a:Type):
    maxLength: nat
    -> ls:list a { length ls <= maxLength }
    -> list a `cost` (maxLength * 2 + 2)
let rev #_ maxLength ls =
    ZL.rev ls
    |> inc ((maxLength - length ls) * 2)

val intersperse(#a:Type):
    maxLength: nat
    -> a
    -> ls:list a { length ls <= maxLength }
    -> list a `cost` (maxLength * 4 + 4)
let rec intersperse #_ maxLength x ls =
    ZL.intersperse x ls
    |> inc ((maxLength - length ls) * 4)

(* List Reductions *)
val foldT(#a #s:Type)(#n:nat):
    maxLength: nat
    -> (s -> a -> s `cost` n)
    -> s
    -> ls:list a { length ls <= maxLength }
    -> Tot (s `cost` (maxLength * (n + 4) + 4))
           (decreases (length ls))
let rec foldT #a #s #n maxLength f b ls =
    ZL.foldT f b ls
    |> inc ((maxLength - length ls) * (n + 4))

val fold(#a #s:Type):
    maxLength: nat
    -> (s -> a -> s)
    -> s
    -> ls:list a { length ls <= maxLength }
    -> s `cost` (maxLength * 4 + 4)
let fold #a #s maxLength f b ls =
    ZL.fold #a #s f b ls
    |> inc ((maxLength - length ls) * 4)


(* Special Folds *)
val sum:
    maxLength: nat
    -> ls:list int { length ls <= maxLength }
    -> int `cost` (maxLength * 4 + 4)
let sum maxLength ls =
    ZL.sum ls
    |> inc ((maxLength - length ls) * 4)

val sumBy(#a:Type):
    maxLength: nat
    -> (a -> int)
    -> ls: list a { length ls <= maxLength }
    -> int `cost` (maxLength * 6 + 6)
let sumBy #_ maxLength f ls =
    ZL.sumBy f ls
    |> inc ((maxLength - length ls) * 6)

val sumByT(#a:Type)(#n:nat):
    maxLength: nat
    -> (a -> int `cost` n)
    -> ls: list a { length ls <= maxLength }
    -> int `cost` (maxLength * (n + 6) + 6)
let sumByT #_ #n maxLength f ls =
    ZL.sumByT f ls
    |> inc ((maxLength - length ls) * (n + 6))

val max :
    maxLength: nat
    -> ls:list int {length ls > 0 /\ length ls <= maxLength}
    -> int `cost` (maxLength * 7 + 7)
let max maxLength ls =
    ZL.max ls
    |> inc ((maxLength - length ls) * 7)
