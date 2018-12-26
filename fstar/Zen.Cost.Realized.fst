module Zen.Cost.Realized

open Zen.Base
abstract noeq type cost (a:Type) (n:nat) =
  | C : inj:a -> cost a n

type t (a:Type) (n:nat) = cost a n

//
// Monadic Return
//

abstract val ret(#a:Type): a ->  cost a 0
abstract let ret #_ x = C x

//
// Monadic Bind
//

abstract val bind(#a #b:Type)(#m #n:nat):
  cost a m -> (a -> cost b n) -> cost b (m+n)
abstract let bind #_ #_ #_ #_ (C x) f = C (C?.inj (f x))

//
// Monadic Increment
//

abstract val inc(#a:Type)(#n: nat): k:nat -> cost a n -> cost a (n+k)
abstract let inc #_ #_ k (C x) = C x

//
// Escape: Cannot be used in programs due to Ghost effect
//

abstract val force(#a:Type)(#n:nat): cost a n -> GTot a
abstract let force #_ #_ (C x) = x

abstract val refine_in(#a:Type)(#n:nat):
    p:(a -> prop)
    -> mx:cost a n
    -> Pure (cost (x:a{p x}) n)
       (requires (p (force mx)))
       (ensures (fun r -> force r == force mx))
abstract let refine_in #a #n p (C x) = C x

abstract val refine_out(#a:Type)(#n:nat):
    p:(a -> prop)
    -> mx:cost (x:a{p x}) n
    -> Pure (cost a n)
       (requires True)
       (ensures (fun r -> p (force r) /\ force r == force mx))
abstract let refine_out #a #n p (C x) = C x

//
// Monad Laws
//

val left_id(#a #b:Type)(#n:nat): x:a -> f:(a -> cost b n)
  -> Lemma (ret x `bind` f == f x )
let left_id #_ #_ #_ _ _ = ()
val right_id(#a:Type)(#n:nat): mx:cost a n
  -> Lemma (mx `bind` ret == mx)
let right_id #_ #_ _ = ()
val assoc(#a #b #c:Type)(#n1 #n2 #n3:nat):
  m:cost a n1 -> f:(a -> cost b n2) -> g:(b -> cost c n3)
  -> Lemma ((m `bind` f) `bind` g == m `bind` (fun x -> f x `bind` g))
let assoc #_ #_ #_ #_ #_ #_ _ _ _ = ()

//
// Lemmas about force
//

val force_ret(#a:Type): x:a -> Lemma (force (ret x) == x)
                               [SMTPat (ret x)]
let force_ret #_ x = ()
val force_inc(#a:Type)(#m:nat):
  n:nat -> mx: cost a m -> Lemma(force (inc n mx) == force mx)
                           [SMTPat (inc n mx)]
let force_inc #_ #_ _ _ = ()
val force_bind(#a #b:Type)(#m #n:nat):
    mx: cost a m
    -> f:(a -> cost b n)
    -> Lemma(force (f (force mx)) == force (bind mx f))
       [SMTPat (bind mx f)]
let force_bind #_ #_ #_ #_ _ _ = ()

val force_bind_inc(#a #b:Type)(#m #n:nat):
    mx:cost a m
    -> f:(a -> cost b n)
    -> Lemma (mx `bind` f == inc m (f (force mx)))
let force_bind_inc #_ #_ #_ #_ _ _ = ()
