module FStar.Char

module Cost = Zen.Cost.Realized
module U8 = FStar.UInt8

type char = U8.t
type t = char

let lowercase (c:char) : Cost.t<char, unit> =
    lazy ( if c > 64uy && c < 91uy
           then c + 32uy
           else c )
    |> Cost.C

let uppercase (c:char) : Cost.t<char, unit> =
    lazy ( if c > 96uy && c < 123uy
           then c - 32uy
           else c )
    |> Cost.C
