module FStar.Pervasives.Native
open Prims

type option (a:Type) =
  | None : option a
  | Some : v:a -> option a

(* 'a * 'b *)
type tuple2 'a 'b =
  | Mktuple2: _1:'a -> _2:'b -> tuple2 'a 'b

let fst (x:tuple2 'a 'b) :'a = Mktuple2?._1 x

let snd (x:tuple2 'a 'b) :'b = Mktuple2?._2 x

(* 'a * 'b * 'c *)
type tuple3 'a 'b 'c =
  | Mktuple3: _1:'a
           -> _2:'b
           -> _3:'c
          -> tuple3 'a 'b 'c

(* 'a * 'b * 'c * 'd *)
type tuple4 'a 'b 'c 'd =
  | Mktuple4: _1:'a
           -> _2:'b
           -> _3:'c
           -> _4:'d
           -> tuple4 'a 'b 'c 'd

(* 'a * 'b * 'c * 'd * 'e *)
type tuple5 'a 'b 'c 'd 'e =
  | Mktuple5: _1:'a
           -> _2:'b
           -> _3:'c
           -> _4:'d
           -> _5:'e
           -> tuple5 'a 'b 'c 'd 'e

(* 'a * 'b * 'c * 'd * 'e * 'f *)
type tuple6 'a 'b 'c 'd 'e 'f =
  | Mktuple6: _1:'a
           -> _2:'b
           -> _3:'c
           -> _4:'d
           -> _5:'e
           -> _6:'f
           -> tuple6 'a 'b 'c 'd 'e 'f

(* 'a * 'b * 'c * 'd * 'e * 'f * 'g *)
type tuple7 'a 'b 'c 'd 'e 'f 'g =
  | Mktuple7: _1:'a
           -> _2:'b
           -> _3:'c
           -> _4:'d
           -> _5:'e
           -> _6:'f
           -> _7:'g
           -> tuple7 'a 'b 'c 'd 'e 'f 'g

(* 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h *)
type tuple8 'a 'b 'c 'd 'e 'f 'g 'h =
  | Mktuple8: _1:'a
           -> _2:'b
           -> _3:'c
           -> _4:'d
           -> _5:'e
           -> _6:'f
           -> _7:'g
           -> _8:'h
           -> tuple8 'a 'b 'c 'd 'e 'f 'g 'h


(*********************************************************************************)
(* Marking terms for normalization *)
(*********************************************************************************)
(*)
abstract let normalize_term (#a:Type) (x:a) : a = x
abstract let normalize (a:Type0) :Type0 = a

abstract
noeq type norm_step =
  | Simpl
  | Weak
  | HNF
  | Primops
  | Delta
  | Zeta
  | Iota
  | UnfoldOnly:list string -> norm_step // each string is a fully qualified name like `A.M.f`
  | UnfoldFully:list string -> norm_step // idem
  | UnfoldAttr:#t:Type0 -> a:t -> norm_step

// Helpers, so we don't expose the actual inductive
abstract let simplify : norm_step = Simpl
abstract let weak     : norm_step = Weak
abstract let hnf      : norm_step = HNF
abstract let primops  : norm_step = Primops
abstract let delta    : norm_step = Delta
abstract let zeta     : norm_step = Zeta
abstract let iota     : norm_step = Iota
abstract let delta_only (s:list string) : norm_step = UnfoldOnly s
abstract let delta_fully (s:list string) : norm_step = UnfoldFully s
abstract let delta_attr (#t:Type)(a:t) : norm_step = UnfoldAttr a

// Normalization marker
abstract let norm (s:list norm_step) (#a:Type) (x:a) : a = x

abstract val assert_norm : p:Type -> Pure unit (requires (normalize p)) (ensures (fun _ -> p))
let assert_norm p = ()

let normalize_term_spec (#a: Type) (x: a) : Lemma (normalize_term #a x == x) = ()
let normalize_spec (a: Type0) : Lemma (normalize a == a) = ()
let norm_spec (s: list norm_step) (#a: Type) (x: a) : Lemma (norm s #a x == x) = ()
