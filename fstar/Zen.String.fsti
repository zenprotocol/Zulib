module Zen.String

open Zen.Base
open Zen.Cost
module A = Zen.Array
module Char = Zen.Char

type t = string

let length = strlen

val string_is_bytearray: unit -> Lemma (string == A.t Char.t)

val toChars:
    s: string
    -> Pure (A.t Char.t)
       (requires True)
       (ensures (fun chars -> chars === s))
val ofChars:
    chars: A.t Char.t
    -> Pure string
       (requires True)
       (ensures (fun s -> s === chars))

val length_eqiv: s:string -> Lemma (length s == A.length (toChars s))

val at:
    s:string
    -> n:nat
    -> Pure string
       (requires (n < length s))
       (ensures (fun result ->
           length result == 1 /\
           begin
           let chars: Char.t `A.indexed` length s =
               length_eqiv s; toChars s in
           let resultChars: Char.t `A.indexed` 1 =
               length_eqiv result; toChars result in
           A.item 0 resultChars == A.item n chars
           end))

private val strcat: string -> string -> string
val cat: s1:string -> s2:string -> string `cost` (length s1 + length s2 + 2)
