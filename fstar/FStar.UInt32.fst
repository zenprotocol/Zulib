module FStar.UInt32

open Zen.Integers
open Zen.Option

let fits (x:int): bool = 0 <= x && x <= 4294967295
let size (x:int): Type0 = b2t(fits x)
type uint_t = x:int{size x}

abstract type t =
  | Mk: v:uint_t -> t

abstract let v (x:t) : Tot (uint_t) = x.v

abstract let uint_to_t (x:uint_t) : Pure t
  (requires True)
  (ensures (fun y -> v y = x)) = Mk x

let uv_inv (x : t) : Lemma
  (ensures (uint_to_t (v x) == x))
  [SMTPat (v x)] = ()

let vu_inv (x : uint_t) : Lemma
  (ensures (v (uint_to_t x) == x))
  [SMTPat (uint_to_t x)] = ()

let v_inj (x1 x2: t): Lemma
  (requires (v x1 == v x2))
  (ensures (x1 == x2))
  = ()

abstract val add: a:t -> b:t -> Pure t
  (requires (size (v a + v b)))
  (ensures (fun c -> v a + v b = v c))
abstract let add a b = Mk (v a + v b)

abstract val add_mod: a:t -> b:t -> Pure t
  (requires True)
  (ensures (fun c -> (v a + v b) % 4294967296 = v c))
abstract let add_mod a b = Mk ((v a + v b) % 4294967296)

abstract val checked_add: a:t -> b:t -> option t
abstract let checked_add a b = if fits (v a + v b) then Some (Mk (v a + v b)) else None

(* Subtraction primitives *)
abstract val sub: a:t -> b:t -> Pure t
  (requires (size (v a - v b)))
  (ensures (fun c -> v a - v b = v c))
abstract let sub a b = Mk (v a - v b)

abstract val sub_mod: a:t -> b:t -> Pure t
  (requires True)
  (ensures (fun c -> (v a - v b) % 4294967296 = v c))
abstract let sub_mod a b = Mk ((v a - v b) % 4294967296)

abstract val checked_sub: a:t -> b:t -> option t
abstract let checked_sub a b = if fits (v a - v b) then Some (Mk (v a - v b)) else None

(* Multiplication primitives *)
abstract val mul: a:t -> b:t -> Pure t
  (requires (size (v a * v b)))
  (ensures (fun c -> v a * v b = v c))
abstract let mul a b = Mk (v a * v b)

abstract val mul_mod: a:t -> b:t -> Pure t
  (requires True)
  (ensures (fun c -> (v a * v b) % 4294967296 = v c))
abstract let mul_mod a b = Mk ((v a * v b) % 4294967296)

abstract val checked_mul: a:t -> b:t -> option t
abstract let checked_mul a b = if fits (v a * v b) then Some (Mk (v a * v b)) else None

(*
val mul_div: a:t -> b:t -> Pure t
  (requires True)
  (ensures (fun c -> (v a * v b) / pow2 n = v c))
let mul_div a b = Mk (mul_div (v a) (v b))
*)

(* Division primitives *)
abstract val div: a:t -> b:t{v b <> 0} -> Pure t
  (requires (True))
  (ensures (fun c -> v b <> 0 ==> v a / v b = v c))
abstract let div a b = Mk (v a / v b)

abstract val checked_div: a:t -> b:t -> option t
abstract let checked_div a b =
    if v b = 0 then None else
    if fits (v a / v b) then Some (Mk (v a / v b)) else None

(* Modulo primitives *)
abstract val rem: a:t -> b:t{v b <> 0} -> Pure t
  (requires True)
  (ensures (fun c ->
    v a - ((v a / v b) * v b) = v c))
abstract let rem a b = Mk (v a % v b)

(*
(* Bitwise operators *)
val logand: t -> t -> Tot t
let logand a b = Mk (logand (v a) (v b))
val logxor: t -> t -> Tot t
let logxor a b = Mk (logxor (v a) (v b))
val logor: t -> t -> Tot t
let logor a b = Mk (logor (v a) (v b))
val lognot: t -> Tot t
let lognot a = Mk (lognot (v a))
*)

(*
(* Shift operators *)
val shift_right: a:t -> s:UInt32.t -> Pure t
  (requires True)
  (ensures (fun c -> UInt32.v s < n ==> v c = (v a / (pow2 (UInt32.v s)))))
let shift_right a s = Mk (shift_right (v a) (UInt32.v s))

val shift_left: a:t -> s:UInt32.t -> Pure t
  (requires True)
  (ensures (fun c -> UInt32.v s < n ==> v c = ((v a * pow2 (UInt32.v s)) % pow2 n)))
let shift_left a s = Mk (shift_left (v a) (UInt32.v s))
*)

(* Comparison operators *)
let eq  (a:t) (b:t) : bool = v a = v b
let gt  (a:t) (b:t) : bool = v a > v b
let gte (a:t) (b:t) : bool = v a >= v b
let lt  (a:t) (b:t) : bool = v a < v b
let lte (a:t) (b:t) : bool = v a <= v b
(*
assume val eq_mask: a:t -> b:t -> Tot (c:t{(v a = v b ==> v c = pow2 n - 1) /\ (v a <> v b ==> v c = 0)})
assume val gte_mask: a:t -> b:t -> Tot (c:t{(v a >= v b ==> v c = pow2 n - 1) /\ (v a < v b ==> v c = 0)})
*)
(* Infix notations *)
unfold let op_Plus_Hat = add
unfold let op_Plus_Percent_Hat = add_mod
unfold let (+?^) = checked_add
unfold let op_Subtraction_Hat = sub
unfold let op_Subtraction_Percent_Hat = sub_mod
unfold let (-?^) = checked_sub
unfold let op_Star_Hat = mul
unfold let op_Star_Percent_Hat = mul_mod
unfold let ( *?^) = checked_mul
//unfold let op_Star_Slash_Hat = mul_div
unfold let op_Slash_Hat = div
unfold let (/?^) = checked_div
unfold let op_Percent_Hat = rem
//unfold let op_Hat_Hat = logxor
//unfold let op_Amp_Hat = logand
//unfold let op_Bar_Hat = logor
//unfold let op_Less_Less_Hat = shift_left
//unfold let op_Greater_Greater_Hat = shift_right
unfold let op_Equals_Hat = eq
unfold let op_Greater_Hat = gt
unfold let op_Greater_Equals_Hat = gte
unfold let op_Less_Hat = lt
unfold let op_Less_Equals_Hat = lte

(* To input / output constants *)
assume val show: t -> s:string { 1 <= strlen s /\ strlen s <= 10 }
assume val showPad: t -> s:string { strlen s = 10 }
assume val read: string -> t
#set-options "--lax"
//This private primitive is used internally by the
//compiler to translate bounded integer constants
//with a desugaring-time check of the size of the number,
//rather than an expensive verifiation check.
//Since it is marked private, client programs cannot call it directly
//Since it is marked unfold, it eagerly reduces,
//eliminating the verification overhead of the wrapper
private
unfold
let __uint_to_t (x:int) : t
    = uint_to_t x
#reset-options
