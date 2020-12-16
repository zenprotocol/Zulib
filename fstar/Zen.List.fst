module Zen.List

open Zen.Base
open Zen.Cost
module OT = Zen.OptionT

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

val isNull (#a:Type): list a -> bool
let isNull #_ = Nil?

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

val rev(#a:Type): ls:list a -> list a `cost` (2 * length ls + 2)
let rev #_ l = Zen.ListRealized.reverse l

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

val product: ls:list int -> int `cost` (length ls * 4 + 4)
let product = fold (fun x y -> x * y) 1

val productBy(#a:Type):
    (a -> int)
    -> ls: list a
    -> int `cost` (length ls * 6 + 6)
let productBy #_ f ls =
    force_map_length f ls;
    map f ls `bind_dep` (fun values ->
    product values <: cost int (length ls * 4 + 4))

val productByT(#a:Type)(#n:nat):
    (a -> int `cost` n)
    -> ls: list a
    -> int `cost` (length ls * (n + 6) + 6)
let productByT #_ #n f ls =
    force_mapT_length f ls;
    mapT f ls `bind_dep` (fun values ->
    product values <: cost int (length ls * 4 + 4))

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

val nth(#a:Type): ls:list a -> n:nat{n < length ls} -> a `cost` (2 * n + 2)
let rec nth #_ (hd::tl) n =
    if n = 0 then hd |> incRet 2
             else nth tl (n-1) |> inc 2

val tryNth(#a:Type): ls:list a -> n:nat -> option a `cost` (2 * n + 7)
let rec tryNth #_ ls n =
    if n < length ls
    then nth ls n >>= OT.some |> inc 5
    else OT.incNone (2 * n + 7)

val take (#a : Type) :
  (k : nat)
  -> (ls : list a)
  -> list a `cost` ((k `min` length ls) * 18 + 18)
let rec take #_ k ls =
    match ls with
    | [] ->
        [] |> incRet 18
    | (hd :: tl) ->
        if k > 0 then
            begin
            let! r = take (k - 1) tl
            in hd :: r |> incRet 18
            end
        else
            [] |> incRet 18

val drop (#a : Type) :
  (k : nat)
  -> (ls : list a)
  -> list a `cost` ((k `min` length ls) * 12 + 12)
let rec drop #_ k ls =
    Zen.Cost.inc 12
        begin match ls with
        | [] ->
            ls |> ret
        | hd :: tl ->
            if k > 0 then
                drop (k - 1) tl
            else
                ls |> ret
        end

val zip (#a #b : Type) :
    (xs : list a)
    -> (ys : list b {length ys = length xs})
    -> list (a ** b) `cost` (18 * length xs + 18)
let rec zip #_ #_ xs ys =
    match (xs , ys) with
    | ([] , []) ->
        [] |> incRet (18 * length xs + 18)
    | (x :: xs', y :: ys') ->
        let! r = zip xs' ys' in
        (x, y) :: r |> incRet 18

val unzip (#a #b : Type) :
    (ls : list (a ** b))
    -> (list a ** list b) `cost` (15 * length ls + 15)
let rec unzip #_ #_ ls =
    match ls with
    | [] ->
        ([], []) |> incRet (15 * length ls + 15)
    | (x, y) :: ls' ->
        let! xs, ys = unzip ls' in
        (x :: xs, y :: ys) |> incRet 15

val zipWithT (#a #b #c : Type) (#n: nat) :
  (a -> b -> c `cost` n)
  -> (xs : list a)
  -> (ys : list b {length ys = length xs})
  -> list c `cost` ((21 + n) * length xs + 21)
let rec zipWithT #_ #_ #_ #n f xs ys =
    match (xs , ys) with
    | ([] , []) ->
        [] |> incRet ((21 + n) * length xs + 21)
    | (x :: xs' , y :: ys') ->
        let! r = zipWithT f xs' ys' in
        let! fxy = f x y in
        fxy :: r |> incRet 21

val filterT (#a : Type) (#n : nat) :
    (a -> bool `cost` n)
    -> (ls : list a)
    -> list a `cost` (length ls * (n + 17) + 17)
let rec filterT #_ #n p ls =
    match ls with
    | [] ->
        [] |> incRet (length ls * (n + 17) + 17)
    | hd :: tl ->
        let! r = filterT p tl in
        let! b = p hd in
        (if b then hd :: r else r)
        |> incRet 17

val chooseT (#a #b : Type) (#n : nat) :
    (a -> option b `cost` n)
    -> (ls : list a)
    -> list b `cost` ((n + 16) * length ls + 16)
let rec chooseT #a #b #n (f : a -> option b `cost` n) (ls : list a) = // 16
    match ls with
    | [] ->
        incRet ((n + 16) * length ls + 16) []
    | hd :: tl ->
        let! r = chooseT f tl in // ((n + 16) * (length ls - 1) + 16)
        let! res = f hd in // n
        begin match res with
        | None ->
            incRet 16 r
        | Some x ->
            incRet 16 (x :: r)
        end
