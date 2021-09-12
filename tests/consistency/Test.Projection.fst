module Test.Projection

module U64 = FStar.UInt64
module Str = FStar.String

let test1 (x:option U64.t {Some? x}) : U64.t =
    Some?.v x `U64.add` 0UL

let test2 (x:result U64.t {OK? x}) : U64.t =
    OK?.v x `U64.add` 0UL

let test3 (x:result U64.t {ERR? x}) : nat =
    Str.strlen (ERR?.msg x)

let test4 (x:U64.t ** U64.t ** U64.t) : U64.t =
    Mktuple3?._1 x

let test5 (x:U64.t ** U64.t) : U64.t =
    Mktuple2?._1 x

let test6 (x:U64.t ** U64.t) : U64.t =
    Mktuple2?._2 x
