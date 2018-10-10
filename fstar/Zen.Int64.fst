module Zen.Int64

open Zen.Integers
open Zen.Option

let fits (x:int): bool = -9223372036854775808 <= x && x <= 9223372036854775807
let size (x:int): Type0 = b2t(fits x)
type int_t = x:int{size x}

abstract type t =
    | Mk: v:int_t -> t

abstract let v (x:t) : int_t = x.v

abstract val int_to_t: x:int_t -> Pure t
  (requires True)
  (ensures (fun y -> v y = x))
abstract let int_to_t x = Mk x

let uv_inv (x : t) : Lemma
  (ensures (int_to_t (v x) == x))
  [SMTPat (v x)] = ()

let vu_inv (x : int_t) : Lemma
  (ensures (v (int_to_t x) == x))
  [SMTPat (int_to_t x)] = ()

let v_inj (x1 x2: t): Lemma
  (requires (v x1 == v x2))
  (ensures (x1 == x2))
  = ()

abstract val add: a:t -> b:t -> Pure t
  (requires (size (v a + v b)))
  (ensures (fun c -> v a + v b = v c))
abstract let add a b = Mk ((v a) + (v b))

abstract val add_mod: a:t -> b:t -> Pure t
  (requires True)
  (ensures (fun c -> (v a + v b) @% 18446744073709551616 = v c))
abstract let add_mod a b = Mk ((v a + v b) @% 18446744073709551616)

abstract val checked_add: a:t -> b:t -> option t
abstract let checked_add a b = if fits (v a + v b) then Some (add a b) else None

(* Subtraction primitives *)
abstract val sub: a:t -> b:t -> Pure t
  (requires (size (v a - v b)))
  (ensures (fun c -> v a - v b = v c))
abstract let sub a b = Mk (v a - v b)

abstract val sub_mod: a:t -> b:t -> Pure t
  (requires True)
  (ensures (fun c -> (v a - v b) @% 18446744073709551616 = v c))
abstract let sub_mod a b = Mk ((v a - v b) @% 18446744073709551616)

abstract val checked_sub: a:t -> b:t -> option t
abstract let checked_sub a b = if fits (v a - v b) then Some (sub a b) else None

(* Multiplication primitives *)
abstract val mul: a:t -> b:t -> Pure t
  (requires (size (v a * v b)))
  (ensures (fun c -> v a * v b = v c))
abstract let mul a b = Mk (v a * v b)

abstract val mul_mod: a:t -> b:t -> Pure t
  (requires True)
  (ensures (fun c -> (v a * v b) @% 18446744073709551616 = v c))
abstract let mul_mod a b = Mk ((v a * v b) @% 18446744073709551616)

abstract val checked_mul: a:t -> b:t -> option t
abstract let checked_mul a b = if fits (v a * v b) then Some (mul a b) else None

(*
val mul_div: a:t -> b:t -> Pure t
  (requires True)
  (ensures (fun c -> (v a * v b) / 18446744073709551616 = v c))
let mul_div a b = Mk (mul_div (v a) (v b))
*)

(* Division primitives *)
abstract val div: a:t -> b:t{v b <> 0} -> Pure t
  (requires (size (v a `div_floor` v b)))
  (ensures (fun c -> v a `div_floor` v b = v c))
abstract let div a b = Mk (v a `div_floor` v b)

abstract val checked_div: a:t -> b:t -> option t
abstract let checked_div a b =
    if v b = 0 then None else
    if fits (v a `div_floor` v b) then Some (div a b) else None

(* Modulo primitives *)
#set-options "--z3rlimit 13616400"
val mod: a:int_t -> b:int_t{b <> 0} -> int_t
let mod a b = a - ((a`div_floor`b) * b)
#reset-options

abstract val rem: a:t -> b:t{v b <> 0} -> Pure t
  (requires True)
  (ensures (fun c ->
    v a - ((v a `div_floor` v b) * v b) = v c))
abstract let rem a b = Mk (mod (v a) (v b))


(* Bitwise operators *)
(*
abstract val logand: t -> t -> Tot t
abstract let logand a b = Mk (logand (v a) (v b))
abstract val logxor: t -> t -> Tot t
abstract let logxor a b = Mk (logxor (v a) (v b))
abstract val logor: t -> t -> Tot t
abstract let logor a b = Mk (logor (v a) (v b))
abstract val lognot: t -> Tot t
abstract let lognot a = Mk (lognot (v a))
*)

(* Comparison operators *)

let eq  (a:t) (b:t) : bool = v a = v b
let gt  (a:t) (b:t) : bool = v a > v b
let gte (a:t) (b:t) : bool = v a >= v b
let lt  (a:t) (b:t) : bool = v a < v b
let lte (a:t) (b:t) : bool = v a <= v b
(*
assume val eq_mask: a:t -> b:t -> Tot (c:t{(v a = v b ==> v c = 18446744073709551616 - 1) /\ (v a <> v b ==> v c = 0)})
assume val gte_mask: a:t -> b:t -> Tot (c:t{(v a >= v b ==> v c = 18446744073709551616 - 1) /\ (v a < v b ==> v c = 0)})
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
assume val to_string: t -> string
assume val of_string: string -> t

private
unfold
let __uint_to_t (x:int) : t
    = uint_to_t x
