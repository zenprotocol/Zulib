module Zen.String

open FSharp.Core.Operators.Checked

module Char = Zen.Char
module Cost = Zen.Cost.Realized
module Array = Zen.Array.Base

type string = Prims.string
type t = Prims.string

// let strlen (s:string) : Prims.nat = int64 (s.Length)
let length = Prims.strlen

let string_is_bytearray (): Prims.unit = ()

let toChars (s:string): Array.t<Char.t> = s
let ofChars (chars: Array.t<Char.t>): string = chars

let length_eqiv (_:string): unit = ()

let at (s:string) (i:Prims.nat): string = [| s.[int i] |]

let strcat (s1:t) (s2:t): t = Array.append s1 s2
let cat (s1:t) (s2:t) : Cost.t<t, unit> =
    lazy ( strcat s1 s2 ) |> Cost.C
