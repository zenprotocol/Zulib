module Zen.Integers

open Zen.Base
open Zen.Cost

val p2: nat -> GTot pos
let rec p2 = function
    | 0 -> 1
    | n -> 2 * p2 (n - 1)

val lg2: x:pos -> GTot nat
let rec lg2 = function
    | 1 -> 0
    | x -> 1 + lg2 (x/2)

// 'Circular modulo operator : wraps into [-p/2; p/2[
let op_At_Percent (v:int) (p:int{p>0/\ p%2=0}) : int =
  let m = v % p in if m >= p/2 then m - p else m

// 'flooring' division
let div_floor (a:int) (b:int{b <> 0}) : int =
  if (a >= 0 && b < 0) || (a < 0 && b >= 0) then -(abs a / abs b)
  else abs a / abs b

val p2_m_plus_n: m:nat -> n:nat -> Lemma
  (ensures (p2 (m + n) == p2 m * p2 n))
  (decreases n)
let rec p2_m_plus_n m = function
    | 0 -> ()
    | n -> p2_m_plus_n (m+1) (n-1)

val p2_double: n:nat -> Lemma (p2 (2 * n) = p2 n * p2 n)
let p2_double n = p2_m_plus_n n n

val p2_0:   unit -> Lemma (p2   0 == 1)
val p2_1:   unit -> Lemma (p2   1 == 2)
val p2_2:   unit -> Lemma (p2   2 == 4)
val p2_4:   unit -> Lemma (p2   4 == 0x10)
val p2_8:   unit -> Lemma (p2   8 == 0x100)
val p2_16:  unit -> Lemma (p2  16 == 0x10000)
val p2_32:  unit -> Lemma (p2  32 == 0x100000000)
val p2_64:  unit -> Lemma (p2  64 == 0x10000000000000000)
val p2_128: unit -> Lemma (p2 128 == 0x100000000000000000000000000000000)

let p2_0   () = ()
let p2_1   () = ()
let p2_2   () = p2_1();  p2_double 1
let p2_4   () = p2_2();  p2_double 2
let p2_8   () = p2_4();  p2_double 4
let p2_16  () = p2_8();  p2_double 8
let p2_32  () = p2_16(); p2_double 16
let p2_64  () = p2_32(); p2_double 32
let p2_128 () = p2_64(); p2_double 64

val p2_values: x:nat -> Lemma
  ( let p = p2 x in
    match x with
    |   0 -> p==1
    |   1 -> p==2
    |   8 -> p==0x100
    |  16 -> p==0x10000
    |  32 -> p==0x100000000
    |  64 -> p==0x10000000000000000
    | 128 -> p==0x100000000000000000000000000000000
    | _  -> True )
let p2_values = function
   |   0 -> p2_0()
   |   1 -> p2_1()
   |   8 -> p2_8()
   |  16 -> p2_16()
   |  32 -> p2_32()
   |  64 -> p2_64()
   | 128 -> p2_128()
   |   _ -> ()
