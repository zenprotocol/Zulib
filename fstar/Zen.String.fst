module Zen.String

open FStar.String
open Zen.Cost

module A = Zen.Array
module Byte = FStar.UInt8
module Char = FStar.Char

type t = string

val str_access: s:string -> n:nat
    -> Lemma (n < length s ==> length (s `at` n) == 1)
let str_access _ _ = ()

val memType0(#a:Type):
    a
    -> l:list a
    -> Ghost Type0
       (requires (Prims.length l > 0))
       (ensures (fun _ -> True))
let rec memType0 #_ x = function
    | [hd] -> x == hd
    | hd::tl -> x == hd \/ memType0 x tl


val is_base_16_char: s:string -> Ghost Type0 (requires (length s == 1))
                                             (ensures (fun _ -> True))
let is_base_16_char s =
    memType0 s ["0";"1";"2";"3";"4";"5";"6";"7";"8";"9";
                "a";"b";"c";"d";"e";"f";
                "A";"B";"C";"D";"E";"F"]

val is_base16_str: string -> GTot Type0
let is_base16_str s =
    forall (n:nat).
        n < length s ==> is_base_16_char (s `at` n)
