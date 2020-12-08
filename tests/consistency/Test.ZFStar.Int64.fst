module Test.ZFStar.Int64

open Zen.Integers
open Zen.Option

module I64 = FStar.Int64

open FStar.Int64

let megatest1 : int_t =
    let x = int_to_t (v (int_to_t 5)) in
    let y = 
      (x `add` (x `add_mod` (x `sub_mod` x)))
      `sub`
      (x `mul` x)
    in
    let za = 
      match checked_add x y with
      | Some r -> r | None -> int_to_t 0
    in
    let zs = 
      match checked_sub za x with
      | Some r -> r | None -> int_to_t 1
    in
    let x1 = 
      mul_mod zs za
    in
    let x2 =
      match checked_mul x1 za with
      | Some r -> r | None -> int_to_t 0
    in
    let x3 =
      div x2 (int_to_t 1)
    in
    let x4 =
      match checked_div x3 (int_to_t 1) with
      | Some r -> r | None -> int_to_t 1
    in
    v x2

let megatest2 : int_t =
    let x = int_to_t (v (int_to_t 5)) in
    let y = 
      (x +^ (x +%^ (x -%^ x)))
      -^
      (x *^ x)
    in
    v (y `rem` x)

let test_string : t =
  let x : t = int_to_t 5 in
  match of_string (to_string x) with
  | None -> (int_to_t 0) | Some y -> y

(*)

(* Modulo primitives *)
val mod: a:int_t -> b:int_t{b <> 0} -> int_t
let mod a b = a - ((a`div_floor`b) * b)

