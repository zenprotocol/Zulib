module Zen.List

open Zen.Base
open Zen.Cost

module OT = Zen.OptionT
module P = Prims

type t : Type -> Type = list

val length_cons: #a: Type -> hd: a -> tl: list a ->
  Lemma (length (hd :: tl) == length tl + 1) [SMTPat (hd :: tl)]
let rec length_cons #_ _ _ = ()

val head: #a: Type -> ls: list a {length ls >= 1} -> a
let head #_ (hd :: _) = hd

val tail: #a: Type -> ls: list a {length ls >= 1} -> list a
let tail #_ (_ :: tl) = tl

val cons: #a: Type -> a -> list a -> list a
let cons #_ hd tl = (Zen.Cost.inc 2 (hd :: tl))

val op_Colon_Colon: #a: Type -> a -> list a -> list a
let op_Colon_Colon #_ = cons

val append: #a: Type -> l1: list a -> list a -> cost (list a) (length l1 * 2 + 2)
let rec append #_ l1 l2 =
  (Zen.Cost.inc 11
      (match l1 with
        | [] -> l2 |> incRet 2
        | hd :: tl -> Cons hd <$> append tl l2 |> inc 2))

unfold
val op_Plus_Plus: #a: Type -> l1: list a -> list a -> cost (list a) (2 * length l1 + 2)
unfold
let op_Plus_Plus #_ = append

val force_append_length: #a: Type -> l1: list a -> l2: list a ->
  Lemma (length (force (append l1 l2)) == length l1 + length l2)
let rec force_append_length #_ l1 l2 =
  match l1 with
  | [] -> ()
  | hd :: tl -> force_append_length tl l2

val init_pure: #a: Type -> #n: nat -> l: nat -> f: (i: nat{i < l} -> cost a n) ->
  cost (list a) (l * (n + 2) + 2)
let rec init_pure #_ #_ l f =
  (Zen.Cost.inc 14
      (match l with
        | 0 -> [] |> incRet 2
        | _ -> Cons <$> f 0 <*> init_pure (l - 1) (fun i -> (Zen.Cost.inc 3 (f (i + 1)))) |> inc 2))

val init: #a: Type -> #n: nat -> l: nat -> f: (nat -> cost a n) -> cost (list a) (l * (n + 2) + 2)
let init #_ #_ l f = (Zen.Cost.inc 2 (init_pure l f))

val init_pure_force_cons: #a: Type -> #n: nat -> l: nat -> f: (i: nat{i < l} -> cost a n) ->
  Lemma
  (let ls = init_pure l f in
    match l with
    | 0 -> force ls == []
    | _ -> force ls == force (f 0) :: force (init_pure (l - 1) (fun i -> f (i + 1))))
let init_pure_force_cons #_ #_ l f =
  match l with
  | 0 -> ()
  | _ ->
    let tl = init_pure (l - 1) (fun i -> f (i + 1)) in
    force_ap (Cons <$> f 0) tl;
    assert (force (Cons <$> f 0 <*> tl) == force (f 0) :: force tl)

val init_pure_length: #a: Type -> #n: nat -> l: nat -> f: (i: nat{i < l} -> cost a n) ->
  Lemma
  (let ls = init_pure l f in
    length (force ls) == l)
let rec init_pure_length #a #n l f =
  init_pure_force_cons l f;
  match l with
  | 0 -> ()
  | _ -> init_pure_length (l - 1) (fun i -> f (i + 1))

val mapT: #a: Type -> #b: Type -> #n: nat -> (a -> cost b n) -> ls: list a ->
  cost (list b) (length ls * (n + 2) + 2)
let rec mapT #_ #b #n f =
  fun _ ->
    (Zen.Cost.inc 13
        (match _ with
          | [] -> [] |> incRet 2
          | hd :: tl -> Cons <$> f hd <*> mapT f tl |> inc 2))

val force_mapT_cons: 
  #a: Type ->
  #b: Type ->
  #n: nat ->
  f: (a -> cost b n) ->
  ls: list a {Cons? ls} ->
  Lemma (match ls with | hd :: tl -> (force (mapT f ls) == force (Cons <$> f hd <*> mapT f tl)))
let force_mapT_cons #_ #_ #_ f (hd :: tl) = force_inc 2 (Cons <$> f hd <*> mapT f tl)

val force_mapT_length: #a: Type -> #b: Type -> #n: nat -> f: (a -> cost b n) -> ls: list a ->
  Lemma
  (let result = mapT f ls in
    length (force result) == length ls)
let rec force_mapT_length #_ #_ #_ f ls =
  match ls with
  | [] -> ()
  | hd :: tl ->
    force_mapT_cons f ls;
    force_ap (Cons <$> f hd) (mapT f tl);
    force_mapT_length f tl;
    ()

val map: #a: Type -> #b: Type -> (a -> b) -> ls: list a -> cost (list b) (length ls * 2 + 2)
let map #_ #_ f = (Zen.Cost.inc 3 (mapT (f >> ret)))

val force_map_length: #a: Type -> #b: Type -> f: (a -> b) -> ls: list a ->
  Lemma
  (let result = map f ls in
    length (force result) == length ls)
let force_map_length #_ #_ f = force_mapT_length (f >> ret)

val revAppend: #a: Type -> l1: list a -> list a -> cost (list a) (2 * length l1 + 2)
let rec revAppend #_ l1 l2 =
  (Zen.Cost.inc 10
      (match l1 with
        | [] -> l2 |> incRet 2
        | hd :: tl -> revAppend tl (hd :: l2) |> inc 2))

val force_revAppend_length: #a: Type -> l1: list a -> l2: list a ->
  Lemma (length (force (revAppend l1 l2)) == length l1 + length l2)
let rec force_revAppend_length #_ l1 l2 =
  match l1 with
  | [] -> ()
  | hd :: tl -> force_revAppend_length tl (hd :: l2)

val rev: #a: Type -> ls: list a -> cost (list a) (2 * length ls + 2)
let rev #_ l = (Zen.Cost.inc 2 (revAppend l []))

val force_rev_length: #a: Type -> ls: list a -> Lemma (length (force (rev ls)) == length ls)
let force_rev_length #_ l = force_revAppend_length l []

val intersperse: #a: Type -> a -> ls: list a -> cost (list a) (4 * length ls + 4)
let rec intersperse #_ x =
  fun _ ->
    (Zen.Cost.inc 15
        (match _ with
          | [] -> [] |> incRet 4
          | [e] -> [e] |> incRet 8
          | hd :: tl -> let! tl = intersperse x tl in hd :: x :: tl |> incRet 4))

val force_intersperse_length: #a: Type -> x: a -> ls: list a ->
  Lemma
  (length (force (intersperse x ls)) ==
    (match length ls with
      | 0 | 1 -> length ls
      | _ -> length ls * 2 - 1))
let rec force_intersperse_length #_ x =
  function
  | [] | [_] -> ()
  | _ :: tl -> force_intersperse_length x tl

val foldT: #a: Type -> #s: Type -> #n: nat -> (s -> a -> cost s n) -> s -> ls: list a ->
  Tot (cost s (length ls * (n + 4) + 4)) (decreases (length ls))
let rec foldT #_ #_ #_ f s =
  fun _ ->
    (Zen.Cost.inc 13
        (match _ with
          | [] -> s |> incRet 4
          | hd :: tl -> let! s = f s hd in foldT f s tl |> inc 4))

val fold: #a: Type -> #s: Type -> (s -> a -> s) -> s -> ls: list a -> cost s (4 * length ls + 4)
let fold #_ #_ f = (Zen.Cost.inc 1 (foldT (fun state x -> (Zen.Cost.inc 4 (f state x |> ret)))))

val sum: ls: list int -> cost int (length ls * 4 + 4)
let sum = (Zen.Cost.inc 2 (fold ( + ) 0))

val sumBy: #a: Type -> (a -> int) -> ls: list a -> cost int (length ls * 6 + 6)
let sumBy #_ f ls =
  (Zen.Cost.inc 6
      (force_map_length f ls;
        bind_dep (map f ls)
          (fun values -> (Zen.Cost.inc 1 (sum values <: cost int (length ls * 4 + 4))))))

val sumByT: #a: Type -> #n: nat -> (a -> cost int n) -> ls: list a ->
  cost int (length ls * (n + 6) + 6)
let sumByT #_ #n f ls =
  (Zen.Cost.inc 6
      (force_mapT_length f ls;
        bind_dep (mapT f ls)
          (fun values -> (Zen.Cost.inc 1 (sum values <: cost int (length ls * 4 + 4))))))

val or_: ls: list bool -> cost bool (length ls * 4 + 4)
let or_ = (Zen.Cost.inc 2 (fold ( || ) true))

val any: #a: Type -> (a -> bool) -> ls: list a -> cost bool (length ls * 6 + 6)
let any #_ f ls =
  (Zen.Cost.inc 6
      (force_map_length f ls;
        bind_dep (map f ls)
          (fun values -> (Zen.Cost.inc 1 (or_ values <: cost bool (length ls * 4 + 4))))))

val anyT: #a: Type -> #n: nat -> (a -> cost bool n) -> ls: list a ->
  cost bool (length ls * (n + 6) + 6)
let anyT #_ #n f ls =
  (Zen.Cost.inc 6
      (force_mapT_length f ls;
        bind_dep (mapT f ls)
          (fun values -> (Zen.Cost.inc 1 (or_ values <: cost bool (length ls * 4 + 4))))))

val and_: ls: list bool -> cost bool (length ls * 4 + 4)
let and_ = (Zen.Cost.inc 2 (fold ( && ) true))

val all: #a: Type -> (a -> bool) -> ls: list a -> cost bool (length ls * 6 + 6)
let all #_ f ls =
  (Zen.Cost.inc 6
      (force_map_length f ls;
        bind_dep (map f ls)
          (fun values -> (Zen.Cost.inc 1 (and_ values <: cost bool (length ls * 4 + 4))))))

val allT: #a: Type -> #n: nat -> (a -> cost bool n) -> ls: list a ->
  cost bool (length ls * (n + 6) + 6)
let allT #_ #n f ls =
  (Zen.Cost.inc 6
      (force_mapT_length f ls;
        bind_dep (mapT f ls)
          (fun values -> (Zen.Cost.inc 1 (and_ values <: cost bool (length ls * 4 + 4))))))

val max: ls: list int {length ls > 0} -> cost int (length ls * 7 + 7)
let max ls =
  (Zen.Cost.inc 7
      (foldT (fun max x -> (Zen.Cost.inc 8 ((if x > max then x else max) |> incRet 3))) (head ls) ls |>
        inc 3))

val nth: #a: Type -> ls: list a -> n: nat{n < length ls} -> cost a (2 * n + 2)
let rec nth #_ (hd :: tl) n =
  (Zen.Cost.inc 12 (if n = 0 then hd |> incRet 2 else nth tl (n - 1) |> inc 2))

val tryNth: #a: Type -> ls: list a -> n: nat -> cost (option a) (2 * n + 7)
let rec tryNth #_ ls n =
  (Zen.Cost.inc 13 (if n < length ls then nth ls n >>= OT.some |> inc 5 else OT.incNone (2 * n + 7))
  )

val filter: #a: Type -> (a -> bool) -> ls: list a -> cost (list a) (4 * length ls + 4)
let filter #_ f =
  (Zen.Cost.inc 2 (fold (fun r x -> (Zen.Cost.inc 6 (if f x then x :: r else r))) []))

val zip: #a: Type -> #b: Type -> ls1: list a -> ls2: list b ->
  cost (list (a ** b)) (P.min (length ls1) (length ls2))
let rec zip #_ #_ ls1 ls2 =
  (Zen.Cost.inc 20
      (match (ls1, ls2) with
        | [], _ -> [] |> incRet (P.min (length ls1) (length ls2))
        | _, [] -> [] |> incRet (P.min (length ls1) (length ls2))
        | x :: xs, y :: ys -> (let! r = zip xs ys in (x, y) :: r |> incRet)))
val mainFunction: Zen.Types.mainFunction
let mainFunction = Zen.Types.MainFunc (Zen.Types.CostFunc cf) main