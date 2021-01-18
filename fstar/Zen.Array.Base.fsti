module Zen.Array.Base

open Zen.Cost

type t (a:Type) = array a

(** Gets the length of the array *)
val length(#a:Type): array a -> nat

type indexed (a:Type) (l:nat) = arr:array a{length arr == l}

(** accesses an array at i *)
val get(#a:Type)(#l:nat): arr:indexed a l -> i:nat{i < l} -> a

(** The empty array *)
val empty: indexed 'a 0

(** The empty array is the unique array with length 0 *)
val empty_uniq: unit -> Lemma (forall (a:Type) (arr: array a).
                               length arr == 0 <==> arr == empty)

(** Converts a list to an array *)
val ofList(#a:Type):
    ls:list a
    -> indexed a (Prims.length ls) `cost` (Prims.length ls * 2 + 2)

(** The element of `Array.ofList ls` at `i` is equal to `List.nth ls i` *)
val ofList_get(#a:Type):
    ls:list a
    -> i:nat{i < Prims.length ls}
    -> Lemma (let arr = ofList ls in
              get (force arr) i == force (Zen.List.nth ls i))

val fold(#a #s:Type):
    (s -> a -> s)
    -> s
    -> arr:t a
    -> s `cost` (4 * length arr + 4)

val foldT(#a #s:Type)(#n:nat):
    (s -> a -> s `cost` n)
    -> s
    -> arr:t a
    -> s `cost` (length arr * (n + 4) + 4)
