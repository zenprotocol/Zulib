module Zen.List

open Zen.Base
open Zen.Cost

module OT = Zen.OptionT
module P = Prims

type t : Type -> Type = list

val length_cons(#a:Type): hd:a -> tl:list a
    -> Lemma (length (hd::tl) == length tl + 1)
    [SMTPat (hd::tl)]
let rec length_cons #_ _ _ = ()

(* Basic Functions *)

val head(#a:Type): ls:list a{length ls >= 1} -> a
let head #_ (hd::_) = hd

val tail(#a:Type): ls:list a{length ls >= 1} -> list a
let tail #_ (_::tl) = tl

val cons(#a:Type): a -> list a -> list a
let cons #_ hd tl = hd::tl

val (::) (#a:Type): a -> list a -> list a
let (::) #_ = cons

(* val isNull (#a:Type): list a -> bool
let isNull #_ = Nil? *)

val append(#a:Type):
    l1:list a
    -> list a
    -> list a `cost` (length l1 * 2 + 2)
let rec append #_ l1 l2 = match l1 with
    | [] -> l2 |> incRet 2
    | hd::tl ->
        Cons hd <$> append tl l2
        |> inc 2

unfold val (++) (#a:Type): l1:list a -> list a -> list a `cost` (2 * length l1 + 2)
unfold let (++) #_ = append

val force_append_length(#a:Type):
    l1:list a
    -> l2:list a
    -> Lemma (length (force (append l1 l2)) == length l1 + length l2)
let rec force_append_length #_ l1 l2 = match l1 with
    | [] -> ()
    | hd::tl -> force_append_length tl l2

val init_pure(#a:Type)(#n:nat):
    l:nat
    -> f:(i:nat{i < l} -> a `cost` n)
    -> list a `cost` (l * (n + 2) + 2)
let rec init_pure #_ #_ l f =
    match l with
    | 0 -> [] |> incRet 2
    | _ -> Cons <$> f 0 <*> init_pure (l - 1) (fun i -> f (i + 1))
           |> inc 2

val init(#a:Type)(#n:nat):
    l:nat
    -> f:(nat -> a `cost` n)
    -> list a `cost` (l * (n + 2) + 2)
let init #_ #_ l f = init_pure l f

val init_pure_force_cons(#a:Type)(#n:nat):
    l:nat
    -> f:(i:nat{i < l} -> a `cost` n)
    -> Lemma (
    let ls = init_pure l f in
    match l with
    | 0 -> force ls == []
    | _ -> force ls == force (f 0) :: force (init_pure (l-1) (fun i -> f (i + 1)))
    )
let init_pure_force_cons #_ #_ l f =
    match l with
    | 0 -> ()
    | _ ->
        let tl = init_pure (l - 1) (fun i -> f (i + 1)) in
        force_ap (Cons <$> f 0) tl;
        assert(force (Cons <$> f 0 <*> tl) == force (f 0) :: force tl)

val init_pure_length(#a:Type)(#n:nat):
    l:nat
    -> f:(i:nat{i < l} -> a `cost` n)
    -> Lemma (let ls = init_pure l f in
              length (force ls) == l)
let rec init_pure_length #a #n l f =
    init_pure_force_cons l f;
    match l with
    | 0 -> ()
    | _ -> init_pure_length (l-1) (fun i -> f (i + 1))

(* List Transformations *)
val mapT(#a #b:Type)(#n:nat):
  (a -> b `cost` n)
  -> ls: list a
  -> list b `cost` (length ls * (n + 2) + 2)
let rec mapT #_ #b #n f = function
    | [] -> [] |> incRet 2
    | hd::tl ->
        Cons <$> f hd <*> mapT f tl
        |> inc 2

val force_mapT_cons(#a #b:Type)(#n:nat):
  f: (a -> b `cost` n)
  -> ls: list a{Cons? ls}
  -> Lemma ( match ls with | hd::tl ->
             (force (f `mapT` ls) == force (Cons <$> f hd <*> mapT f tl)))
let force_mapT_cons #_ #_ #_ f (hd::tl) =
    force_inc 2 (Cons <$> f hd <*> mapT f tl)

val force_mapT_length(#a #b:Type)(#n:nat):
  f: (a -> b `cost` n)
  -> ls: list a
  -> Lemma ( let result = f `mapT` ls in
             length (force result) == length ls )
let rec force_mapT_length #_ #_ #_ f ls = match ls with
    | [] -> ()
    | hd::tl ->
        force_mapT_cons f ls;
        force_ap (Cons <$> f hd) (mapT f tl);
        force_mapT_length f tl;
        ()

val map(#a #b:Type):
    (a -> b)
    -> ls:list a
    -> list b `cost` (length ls * 2 + 2)
let map #_ #_ f = mapT (f >> ret)

val force_map_length(#a #b:Type):
  f: (a -> b)
  -> ls: list a
  -> Lemma ( let result = f `map` ls in
             length (force result) == length ls )
let force_map_length #_ #_ f = force_mapT_length (f>>ret)

val revAppend(#a:Type): l1:list a -> list a
    -> list a `cost` (2 * length l1 + 2)
let rec revAppend #_ l1 l2 = match l1 with
    | [] -> l2 |> incRet 2
    | hd::tl -> revAppend tl (hd::l2) |> inc 2

val force_revAppend_length(#a:Type):
    l1:list a
    -> l2: list a
    -> Lemma (length (force (revAppend l1 l2)) == length l1 + length l2)
let rec force_revAppend_length #_ l1 l2 = match l1 with
    | [] -> ()
    | hd::tl -> force_revAppend_length tl (hd::l2)

val rev(#a:Type): ls:list a -> list a `cost` (2 * length ls + 2)
let rev #_ l = revAppend l []

val force_rev_length(#a:Type):
    ls:list a
    -> Lemma (length (force (rev ls)) == length ls)
let force_rev_length #_ l = force_revAppend_length l []

val intersperse(#a:Type): a -> ls:list a -> list a `cost` (4 * length ls + 4)
let rec intersperse #_ x = function
    | [] -> [] |> incRet 4
    | [e] -> [e] |> incRet 8
    | hd::tl ->
        let! tl = intersperse x tl in
        hd::x::tl |> incRet 4

val force_intersperse_length(#a:Type):
    x:a
    -> ls:list a
    -> Lemma (length (force (intersperse x ls)) == begin match length ls with
                                                   | 0 | 1 -> length ls
                                                   | _ -> length ls * 2 - 1
                                                   end)
let rec force_intersperse_length #_ x = function
    | [] | [_] -> ()
    | _::tl -> force_intersperse_length x tl


(* List Reductions *)
val foldT(#a #s:Type)(#n:nat):
    (s -> a -> s `cost` n)
    -> s
    -> ls:list a
    -> Tot (s `cost` (length ls * (n + 4) + 4))
           (decreases (length ls))
let rec foldT #_ #_ #_ f s = function
    | [] -> s |> incRet 4
    | hd::tl ->
        let! s = f s hd in
        foldT f s tl |> inc 4

val fold(#a #s:Type):
    (s -> a -> s)
    -> s
    -> ls:list a
    -> s `cost` (4 * length ls + 4)
let fold #_ #_ f = foldT (fun state x -> f state x |> ret)

(* Special Folds *)
val sum: ls:list int -> int `cost` (length ls * 4 + 4)
let sum = fold (+) 0

val sumBy(#a:Type):
    (a -> int)
    -> ls: list a
    -> int `cost` (length ls * 6 + 6)
let sumBy #_ f ls =
    force_map_length f ls;
    map f ls `bind_dep` (fun values ->
    sum values <: cost int (length ls * 4 + 4))

val sumByT(#a:Type)(#n:nat):
    (a -> int `cost` n)
    -> ls: list a
    -> int `cost` (length ls * (n + 6) + 6)
let sumByT #_ #n f ls =
    force_mapT_length f ls;
    mapT f ls `bind_dep` (fun values ->
    sum values <: cost int (length ls * 4 + 4))

val or_: ls : list bool -> bool `cost` (length ls * 4 + 4)
let or_ = fold ( || ) true

val any(#a:Type):
    (a -> bool)
    -> ls: list a
    -> bool `cost` (length ls * 6 + 6)
let any #_ f ls =
    force_map_length f ls;
    map f ls `bind_dep` (fun values ->
    or_ values <: cost bool (length ls * 4 + 4))

val anyT(#a:Type)(#n:nat):
    (a -> bool `cost` n)
    -> ls: list a
    -> bool `cost` (length ls * (n + 6) + 6)
let anyT #_ #n f ls =
    force_mapT_length f ls;
    mapT f ls `bind_dep` (fun values ->
    or_ values <: cost bool (length ls * 4 + 4))

val and_: ls: list bool -> bool `cost` (length ls * 4 + 4)
let and_ = fold ( && ) true

val all(#a:Type):
    (a -> bool)
    -> ls: list a
    -> bool `cost` (length ls * 6 + 6)
let all #_ f ls =
    force_map_length f ls;
    map f ls `bind_dep` (fun values ->
    and_ values <: cost bool (length ls * 4 + 4))

val allT(#a:Type)(#n:nat):
    (a -> bool `cost` n)
    -> ls: list a
    -> bool `cost` (length ls * (n + 6) + 6)
let allT #_ #n f ls =
    force_mapT_length f ls;
    mapT f ls `bind_dep` (fun values ->
    and_ values <: cost bool (length ls * 4 + 4))

val max : ls:list int{length ls > 0} -> int `cost` (length ls * 7 + 7)
let max ls = foldT (fun max x -> (if x > max then x else max) |> incRet 3)
                   (head ls)
                   ls
              |> inc 3

val maxBy (#a #b:Type):
    (a -> int)
    -> ls: list a { length ls > 0 }
    -> int `cost` (length ls * 9 + 9)
let maxBy #_ #_ f ls =
    force_map_length f ls;
    bind_dep (map f ls) (fun values ->
    max values <: cost int (length ls * 7 + 7))

val min : ls:list int{length ls > 0} -> int `cost` (length ls * 7 + 7)
let min ls = foldT (fun min x -> (if x < min then x else min) |> incRet 3)
                   (head ls)
                   ls
              |> inc 3

val minBy (#a #b:Type):
    (a -> int)
    -> ls: list a { length ls > 0 }
    -> int `cost` (length ls * 9 + 9)
let minBy #_ #_ f ls =
    force_map_length f ls;
    bind_dep (map f ls) (fun values ->
    min values <: cost int (length ls * 7 + 7))

val nth(#a:Type): ls:list a -> n:nat{n < length ls} -> a `cost` (2 * n + 2)
let rec nth #_ (hd::tl) n =
    if n = 0 then hd |> incRet 2
             else nth tl (n-1) |> inc 2

val tryNth(#a:Type): ls:list a -> n:nat -> option a `cost` (2 * n + 7)
let rec tryNth #_ ls n =
    if n < length ls
    then nth ls n >>= OT.some |> inc 5
    else OT.incNone (2 * n + 7)

val filter(#a:Type):
    (a -> bool)
    -> ls: list a
    -> list a `cost` (4 * length ls + 4)
let filter #_ f = fold (fun r x -> if f x then x :: r else r) []

val sumNat: ls:list nat -> nat `cost` (length ls * 4 + 4)
let sumNat =
    let (+) (x : nat) (y : nat) : nat = x + y
    in fold (+) 0

val sumByNat (#a:Type):
    (a -> n:nat)
    -> ls: list a
    -> nat `cost` (length ls * 6 + 6)
let sumByNat #_ f ls =
    force_map_length f ls;
    map f ls `bind_dep` (fun values ->
    sumNat values <: cost nat (length ls * 4 + 4))

val foldrT_dep_cf (a s:Type) (c:a -> nat):
    ls:list a -> GTot nat
let foldrT_dep_cf a s c ls = force (
    let! r = sumByNat c ls
    in ret <| r + length ls * 4 + 4
    )

val foldrT_dep(#a #s:Type)(#c:a -> nat):
    (x:a -> r:s -> s `cost` c x)
    -> b:s
    -> ls:list a
    -> Tot (s `cost` foldrT_dep_cf a s c ls)
           (decreases (length ls))
let rec foldrT_dep #a #s #c f r = function
    | [] -> ret r |> inc (foldrT_dep_cf a s c [])
    | hd::tl ->
        let! r = f hd r in
        foldrT_dep f r tl |> inc 4

(* val concat (#a:Type):
    ls: list (list a)
    (* -> list a `cost` (length ls * ((2 * (force (max (force (map length ls)))) + 2) + 4) + 4) *)
    -> list a `cost` (length ls * (n + 4) + 4)
(* let concat #a ls = foldT (++) [] ls *)
let rec concat #_ ls = match ls with
    | []       -> ret []
    | (hd::tl) ->
        let! tl' = concat tl
        in ret (hd ++ tl') *)

(* val zip(#a #b:Type):
    ls1 : list a
    -> ls2 : list b
    -> list (a ** b) `cost` (P.min (length ls1) (length ls2) * 20 + 20)
let rec zip #a #_ ls1 ls2 = (match (ls1, ls2) with
    | ([], _) -> ret []
    | (_, []) -> ret []
    | ((x::xs), (y::ys)) ->
        let (xs: list a) = xs in
        let! r = zip xs ys
        in ret ((x,y) :: r))
        |> inc 20 *)

(* unfold val (++) (#a:Type): l1:list a -> list a -> list a `cost` (2 * length l1 + 2)

val foldT(#a #s:Type)(#n:nat):
    (s -> a -> s `cost` n)
    -> s
    -> ls:list a
    -> Tot (s `cost` (length ls * (n + 4) + 4))
           (decreases (length ls)) *)
