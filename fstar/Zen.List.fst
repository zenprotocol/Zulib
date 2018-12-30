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

val tryMapT(#a #b:Type)(#n:nat):
    (a -> option b `cost` n)
    -> ls:list a
    -> option (list b) `cost` (length ls * (n + 2) + 2)
let rec tryMapT #_ #_ #_ f = function
    | [] -> Some [] |> incRet 2
    | hd::tl ->
        let! hd = f hd in
        let! tl = tryMapT f tl in
        begin match hd, tl with
        | Some hd, Some tl -> Some (hd::tl)
        | _ -> None
        end |> incRet 2

val force_tryMapT_cons(#a #b:Type)(#n:nat):
  f: (a -> option b `cost` n)
  -> ls: list a
  -> Lemma begin match ls with
                 | [] -> True
                 | hd::tl ->
                    force (f `tryMapT` ls) ==
                    begin match force (f hd), force (f `tryMapT` tl) with
                    | Some hd, Some tl -> Some (hd::tl)
                    | _ -> None
                    end
           end
let force_tryMapT_cons #_ #_ #_ _ _ = ()

val force_tryMapT_length(#a #b:Type)(#n:nat):
  f: (a -> option b `cost` n)
  -> ls: list a
  -> Lemma (match force (f `tryMapT` ls) with
                 | None -> True
                 | Some r -> length r == length ls)
let rec force_tryMapT_length #_ #_ #_ f = function
    | [] -> ()
    | hd::tl -> force_tryMapT_cons f tl; force_tryMapT_length f tl

val zipWithT(#a #b #c:Type)(#n:nat):
       (a -> b -> c `cost` n)
    -> ls1:list a
    -> ls2:list b
    -> list c `cost` (length ls1 * (n+2) + 2)
let rec zipWithT #_ #_ #_ #n f ls1 ls2 =
    match ls1, ls2 with
    | [], _ -> [] |> incRet 2
    | _, [] -> [] |> incRet (length ls1 * (n+2) + 2)
    | x::xs, y::ys ->
        let! tl = zipWithT f xs ys in
        let! r = f x y in
        r :: tl |> incRet 2

val force_zipWithT_cons(#a #b #c:Type)(#n:nat):
    f: (a -> b -> c `cost` n)
    -> ls1:list a
    -> ls2:list b
    -> Lemma (match ls1, ls2 with
              | [], _ | _, [] -> True
              | x::xs, y::ys ->
                  force (zipWithT f ls1 ls2) == force (f x y)::force (zipWithT f xs ys))
let force_zipWithT_cons #_ #_ #_ #_ _ _ _ = ()

val zipWith(#a #b #c:Type):
       (a -> b -> c)
    -> ls1:list a
    -> ls2:list b
    -> list c `cost` (length ls1 * 2 + 2)
let zipWith #_ #_ #_ f =
    zipWithT (fun x y -> ret (f x y))

val zip(#a #b:Type):
       ls1:list a
    -> ls2:list b
    -> list (a ** b) `cost` (length ls1 * 2 + 2)
let zip #_ #_ = zipWith Mktuple2

val tryZipWithT(#a #b #c:Type)(#n:nat):
    (a -> b -> c `cost` n)
    -> ls1:list a
    -> ls2:list b
    -> option (list c) `cost` (length ls1 * (n + 2) + 2)
let rec tryZipWithT #_ #_ #_ #n f ls1 ls2 =
    match ls1, ls2 with
    | [], _ -> Some [] |> incRet 2
    | _::_, [] -> None |> incRet (length ls1 * (n + 2) + 2)
    | x::xs, y::ys ->
        let! hd = f x y in
        let! tl = tryZipWithT f xs ys in
        begin match tl with
        | Some tl -> Some (hd::tl) |> incRet 2
        | None -> None |> incRet 2
        end

val force_tryZipWithT_cons(#a #b #c:Type)(#n:nat):
    f: (a -> b -> c `cost` n)
    -> ls1:list a
    -> ls2:list b
    -> Lemma (match ls1, ls2 with
              | [], _ | _, [] -> True
              | x::xs, y::ys ->
                  force (tryZipWithT f ls1 ls2) ==
                  begin match force (tryZipWithT f xs ys) with
                  | Some tl -> Some (force (f x y)::tl)
                  | None -> None end)
let force_tryZipWithT_cons #_ #_ #_ #_ _ _ _ = ()

val force_tryZipWithT_length(#a #b #c:Type)(#n:nat):
    f: (a -> b -> c `cost` n)
    -> ls1:list a
    -> ls2:list b
    -> Lemma (match force (tryZipWithT f ls1 ls2) with
              | None -> True
              | Some r -> length r == length ls1)
let rec force_tryZipWithT_length #_ #_ #_ #_ f ls1 ls2 =
    match ls1, ls2 with
    | [], _ | _::_, [] -> ()
    | x::xs, y::ys -> force_tryZipWithT_cons f ls1 ls2;
                      force_tryZipWithT_length f xs ys

val tryZipWith(#a #b #c:Type):
    (a -> b -> c)
    -> ls1:list a
    -> ls2:list b
    -> option (list c) `cost` (length ls1 * 2 + 2)
let tryZipWith #_ #_ #_ f =
    tryZipWithT (fun x y -> ret (f x y))

val tryZip(#a #b #c:Type):
    ls1:list a
    -> ls2:list b
    -> option (list (a **b)) `cost` (length ls1 * 2 + 2)
let tryZip #_ #_ #_ =
    tryZipWith Mktuple2

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

val foldOT(#a #s:Type)(#n:nat):
    (s -> a -> option s `cost` n)
    -> s
    -> ls:list a
    -> Tot (option s `cost` (length ls * (n + 4) + 4))
           (decreases (length ls))
let rec foldOT #_ #s #n f st ls =
    match ls with
    | [] -> Some st |> incRet 4
    | x::xs ->
        let! st = (f st x |> inc 4) <: (option s `cost` (n+4)) in
        begin match st with
        | None -> None |> incRet (length xs * (n + 4) + 4)
        | Some st -> foldOT f st xs
        end

val foldIgnoreT(#a #s:Type)(#n:nat):
    (s -> a -> option s `cost` n)
    -> s
    -> ls:list a
    -> Tot (option s `cost` (length ls * (n + 4) + 4))
           (decreases (length ls))
let rec foldIgnoreT #_ #s #n f st ls =
    match ls with
    | [] -> Some st |> incRet 4
    | x::xs ->
        let! st' = (f st x |> inc 4) <: (option s `cost` (n+4)) in
        begin match st' with
        | None -> foldOT f st xs
        | Some st' -> foldOT f st' xs
        end

(* Special Folds *)
val sum: ls:list int -> int `cost` (length ls * 4 + 4)
let rec sum ls =
    match ls with
    | [] -> 0 |> incRet 4
    | hd :: tl -> let! r = sum tl in hd + r |> incRet 4

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
let rec or_ ls =
    match ls with
    | [] -> false |> incRet 4
    | hd :: tl -> let! r = or_ tl in (hd || r) |> incRet 4

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
let rec and_ ls =
    match ls with
    | [] -> true |> incRet 4
    | hd :: tl -> let! r = and_ tl in (hd && r) |> incRet 4

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
let rec max ls =
    match ls with
    | hd :: [] -> hd |> incRet 14
    | hd :: tl ->
        let! r = max tl in
            (if hd > r then hd else r)
            |> incRet 7

val maxBy (#a #b:Type):
    (a -> int)
    -> ls: list a { length ls > 0 }
    -> int `cost` (length ls * 9 + 9)
let maxBy #_ #_ f ls =
    force_map_length f ls;
    bind_dep (map f ls) (fun values ->
    max values <: cost int (length ls * 7 + 7))

val min : ls:list int{length ls > 0} -> int `cost` (length ls * 7 + 7)
let rec min ls =
    match ls with
    | hd :: [] -> hd |> incRet 14
    | hd :: tl ->
        let! r = min tl in
            (if hd < r then hd else r)
            |> incRet 7

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
let rec filter #_ f ls =
    match ls with
    | [] -> [] |> incRet 4
    | hd :: tl ->
        let! r = filter f tl in
        (if f hd then hd :: r else r) |> incRet 4

val sumNat: ls:list nat -> nat `cost` (length ls * 4 + 4)
let rec sumNat ls =
    match ls with
    | [] -> 0 |> incRet 4
    | hd :: tl -> let! r = sumNat tl in ((hd + r) <: nat) |> incRet 4

val sumByNat (#a:Type):
    (a -> n:nat)
    -> ls: list a
    -> nat `cost` (length ls * 6 + 6)
let sumByNat #_ f ls =
    force_map_length f ls;
    map f ls `bind_dep` (fun values ->
sumNat values <: cost nat (length ls * 4 + 4))
