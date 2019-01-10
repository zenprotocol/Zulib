module Zen.UInt32.Conversion

open FStar.UInt32

module Int64  = FStar.Int64
module UInt64 = FStar.UInt64
module UInt8  = FStar.UInt8

abstract val fromInt64 : x:Int64.t {fits (Int64.v x)} -> t
abstract let fromInt64 x = uint_to_t (Int64.v x)

abstract val fromUInt64 : x:UInt64.t {fits (UInt64.v x)} -> t
abstract let fromUInt64 x = uint_to_t (UInt64.v x)

abstract val fromUInt8 : x:UInt8.t {fits (UInt8.v x)} -> t
abstract let fromUInt8 x = uint_to_t (UInt8.v x)

abstract val tryFromInt64 : x:Int64.t -> option t
abstract let tryFromInt64 x = if fits (Int64.v x) then Some <| uint_to_t (Int64.v x) else None

abstract val tryFromUInt64 : x:UInt64.t -> option t
abstract let tryFromUInt64 x = if fits (UInt64.v x) then Some <| uint_to_t (UInt64.v x) else None

abstract val tryFromUInt8 : x:UInt8.t -> option t
abstract let tryFromUInt8 x = if fits (UInt8.v x) then Some <| uint_to_t (UInt8.v x) else None

abstract val toInt64 : x:t {Int64.fits (v x)} -> Int64.t
abstract let toInt64 x = Int64.int_to_t (v x)

abstract val toUInt64 : x:t {UInt64.fits (v x)} -> UInt64.t
abstract let toUInt64 x = UInt64.uint_to_t (v x)

abstract val toUInt8 : x:t {UInt8.fits (v x)} -> UInt8.t
abstract let toUInt8 x = UInt8.uint_to_t (v x)

abstract val tryToInt64 : x:t -> option Int64.t
abstract let tryToInt64 x = if Int64.fits (v x) then Some <| Int64.int_to_t (v x) else None

abstract val tryToUInt64 : x:t -> option UInt64.t
abstract let tryToUInt64 x = if UInt64.fits (v x) then Some <| UInt64.uint_to_t (v x) else None

abstract val tryToUInt8 : x:t -> option UInt8.t
abstract let tryToUInt8 x = if UInt8.fits (v x) then Some <| UInt8.uint_to_t (v x) else None

val v_fromInt64 : x:Int64.t {fits (Int64.v x)} -> Lemma
    (Int64.v x == v (fromInt64 x))
let v_fromInt64 x = ()

val v_fromUInt64 : x:UInt64.t {fits (UInt64.v x)} -> Lemma
    (UInt64.v x == v (fromUInt64 x))
let v_fromUInt64 x = ()

val v_fromUInt8 : x:UInt8.t {fits (UInt8.v x)} -> Lemma
    (UInt8.v x == v (fromUInt8 x))
let v_fromUInt8 x = ()

val v_toInt64 : x:t {Int64.fits (v x)} -> Lemma
    (Int64.v (toInt64 x) == v x)
let v_toInt64 x = ()

val v_toUInt64 : x:t {UInt64.fits (v x)} -> Lemma
    (UInt64.v (toUInt64 x) == v x)
let v_toUInt64 x = ()

val v_toUInt8 : x:t {UInt8.fits (v x)} -> Lemma
    (UInt8.v (toUInt8 x) == v x)
let v_toUInt8 x = ()

val v_tryFromInt64 : x:Int64.t -> Lemma
    (fits (Int64.v x) ==> Int64.v x == v (Some?.v (tryFromInt64 x)))
let v_tryFromInt64 x = ()

val v_tryFromUInt64 : x:UInt64.t -> Lemma
    (fits (UInt64.v x) ==> UInt64.v x == v (Some?.v (tryFromUInt64 x)))
let v_tryFromUInt64 x = ()

val v_tryFromUInt8 : x:UInt8.t -> Lemma
    (fits (UInt8.v x) ==> UInt8.v x == v (Some?.v (tryFromUInt8 x)))
let v_tryFromUInt8 x = ()

val v_tryToInt64 : x:t -> Lemma
    (Int64.fits (v x) ==> Int64.v (Some?.v (tryToInt64 x)) == v x)
let v_tryToInt64 x = ()

val v_tryToUInt64 : x:t -> Lemma
    (UInt64.fits (v x) ==> UInt64.v (Some?.v (tryToUInt64 x)) == v x)
let v_tryToUInt64 x = ()

val v_tryToUInt8 : x:t -> Lemma
    (UInt8.fits (v x) ==> UInt8.v (Some?.v (tryToUInt8 x)) == v x)
let v_tryToUInt8 x = ()
