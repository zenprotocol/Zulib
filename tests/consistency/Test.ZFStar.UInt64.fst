module Test.ZFStar.UInt64

open Zen.Integers
open Zen.Option

module U64 = FStar.UInt64

open FStar.UInt64

let megatest1 : uint_t =
    let x = uint_to_t (v (uint_to_t 5)) in
    let y = 
      ((x `mul` x) `add` (x `add_mod` (x `sub_mod` x))) `sub` x
    in
    let za = 
      match checked_add x y with
      | Some r -> r | None -> uint_to_t 0
    in
    let zs = 
      match checked_sub za x with
      | Some r -> r | None -> uint_to_t 1
    in
    let x1 = 
      mul_mod zs za
    in
    let x2 =
      match checked_mul x1 za with
      | Some r -> r | None -> uint_to_t 0
    in
    let x3 =
      div x2 (uint_to_t 1)
    in
    let x4 =
      match checked_div x3 (uint_to_t 1) with
      | Some r -> r | None -> uint_to_t 1
    in
    v x4

let megatest2 : uint_t =
    let x = uint_to_t (v (uint_to_t 5)) in
    let y = 
      ((x *^ x) +^ (x +%^ (x -%^ x))) -^ x
    in
    v (y `rem` x)

let test_string : t =
    let x : t = uint_to_t 5 in
    match of_string (to_string x) with
    | None -> (uint_to_t 0) | Some y -> y


