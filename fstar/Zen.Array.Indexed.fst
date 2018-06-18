module Zen.Array.Indexed

open Zen.Base
open Zen.Cost
open Zen.Array.Base

type t (a: Type) (n: nat) = indexed a n

val ofArray(#a:Type): array a -> l:nat & indexed a l
let ofArray #_ arr = (|length arr, arr|)

val toList(#a:Type)(#l:nat):
    indexed a l
    -> ls:list a{Prims.length ls == l} `cost` (l * 2 + 2)
let toList #a #l arr =
    let rec buildList: i:nat{i<=l} -> ls:list a{Prims.length ls == i} = function
        | 0 -> []
        | i -> get arr (l-i) :: buildList (i-1)
    in
    buildList l |> incRet (l * 2 + 2)

unfold val item(#a:Type)(#l:nat): i:nat{i < l} -> arr:indexed a l -> a
unfold let item #_ #_ i arr = get arr i

unfold val op_Array_Access(#a:Type)(#l:nat): arr:indexed a l -> i:nat{i < l} -> a
unfold let op_Array_Access #_ #_ arr i = get arr i

val init_pure(#a:Type)(#n:nat):
    l:nat
    -> f:(i:nat{i<l} -> a `cost` n)
    -> indexed a l `cost` (l * (n+4) + 4)
let init_pure #a #_ l f =
    Zen.List.init_pure_length l f;
    Zen.List.init_pure l f `bind_dep` (fun (ls:list a{Prims.length ls == l}) ->
        Zen.Array.Base.ofList ls <: indexed a l `cost` (l * 2 + 2))

val init(#a:Type)(#n:nat):
    l:nat
    -> f:(nat -> a `cost` n)
    -> indexed a l `cost` (l * (n+4) + 4)
let init #_ #_ l f = init_pure l f

(** create returns an array of length l in which each element is x. *)
val create(#a:Type): l:nat -> x:a -> indexed a l `cost` (l * 4 + 4)
let create(#_) l x = init l (konst x >> ret)

(** sub returns the subrange of arr from i to (i+j). *)
val sub(#a:Type)(#l:nat): arr:indexed a l -> i:nat{i<l} -> j:nat{i+j <= l}
  -> cost (indexed a j) (j * 4 + 4)
let sub #a #l arr i j =
    let rec buildList: k:nat{k <= j} -> ls:list a{Prims.length ls == k} =
        function
        | 0 -> []
        | k -> get arr (i+j-k) :: buildList (k-1)
    in
    (ofList (buildList j) <: indexed a j `cost` (j * 2 + 2))
    |> inc (j * 2 + 2)

(** Creates an array whose elements are the results of applying the supplied function to each of the elements of a supplied array. *)
val map(#a #b:Type)(#n #l:nat): (a -> cost b n) -> indexed a l
  -> cost (indexed b l) (l * (n+4) + 4)
let map #a #_ #_ #l f arr = init_pure l (get arr >> f)


(** `append a1 a2` returns an array containing the elements of a1 followed by the elements of a2. *)
val append(#a:Type)(#l1 #l2:nat): indexed a l1 -> indexed a l2
  -> cost (indexed a (l1+l2)) ((l1+l2) * 4 + 4)
let append #a #l1 #l2 a1 a2 =
  let fetch (i:nat{ i < (l1+l2)}): a =
    if i < l1 then a1.(i) else a2.(i-l1)
  in
  if l1 + l2 = 0 then empty |> incRet 4 else
  init_pure (l1+l2) (fetch >> ret)


(*
(** [concat3 a1 a2 a3] returns an array containing the elements of a1 followed by the elements of a2 followed by the elements of a3. *)
val concat3(#a:Type)(#l1 #l2 #l3:nat): indexed a l1 -> indexed a l2 -> indexed a l3
  -> cost (indexed a (l1+l2+l3)) (2 * (l1+l2+l3))
let concat3 #_ #l1 #l2 #l3 a1 a2 a3 =
  let fetch (i:nat{ i < (l1+l2+l3)}) = if i < l1      then a1.(i)
                                  else if i < l1 + l2 then a2.(i-l1)
                                  else                     a3.(i-l1-l2)
  in
  if (l1+l2+l3) = 0 then ret empty else
  init (l1+l2+l3) fetch
*)
val chunkBySize(#a:Type)(#l:nat):
    size:pos
    -> a `indexed` l
    -> indexed (indexed a size) (l / size)
       `cost`
       ((l/size)*(4*size+8) + 4)
let chunkBySize #a #l size arr =
    let chunk (i: nat{i < (l / size)})
              : indexed a size `cost` (4*size+4) =
              assert(i <= (l/size) - 1);
              sub arr (i * size) size
    in
    if l / size = 0 then empty |> incRet 4 else
    init_pure (l / size) chunk

val joinOptions(#a:Type)(#l:nat):
    indexed (option a) l
    -> option (indexed a l) `cost` (l * 4 + 4)
let joinOptions #a #l arr =
    let rec buildList
        (i:nat{i <= l})
        : option (ls:list a{Prims.length ls == i}) =
        match i with
        | 0 -> Some []
        | i -> begin match get arr (l-i), buildList (i-1) with
               | Some x, Some xs -> Some (x::xs)
               | _ -> None
               end
    in
    match buildList l with
    | Some xs -> (ofList xs <: (indexed a l `cost` (l * 2 + 2)))
                 >>= Zen.OptionT.incSome (l * 2 + 2)
    | None -> Zen.OptionT.incNone (l * 4 + 4)

val tryMap(#a #b:Type)(#l #n:nat):
    (a -> option b `cost` n)
    -> indexed a l
    -> option (indexed b l) `cost` (l * (n + 8) + 8)
let tryMap #_ #_ #_ #_ f arr =
    map f arr >>= joinOptions
