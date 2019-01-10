module Zen.UInt8.Conversion

open FStar.UInt8

module UInt64 = FStar.Int64
module UInt64 = FStar.UInt64
module UInt32 = FStar.UInt32

abstract val fromInt64 : x:Int64.t {fits (Int64.v x)} -> t
abstract let fromInt64 x = uint_to_t (Int64.v x)

abstract val fromUInt64 : x:UInt64.t {fits (UInt64.v x)} -> t
abstract let fromUInt64 x = uint_to_t (UInt64.v x)

abstract val fromUInt32 : x:UInt32.t {fits (UInt32.v x)} -> t
abstract let fromUInt32 x = uint_to_t (UInt32.v x)

abstract val tryFromInt64 : x:Int64.t -> option t
abstract let tryFromInt64 x = if fits (Int64.v x) then Some <| uint_to_t (Int64.v x) else None

abstract val tryFromUInt64 : x:UInt64.t -> option t
abstract let tryFromUInt64 x = if fits (UInt64.v x) then Some <| uint_to_t (UInt64.v x) else None

abstract val tryFromUInt32 : x:UInt32.t -> option t
abstract let tryFromUInt32 x = if fits (UInt32.v x) then Some <| uint_to_t (UInt32.v x) else None

abstract val toInt64 : x:t {Int64.fits (v x)} -> Int64.t
abstract let toInt64 x = Int64.int_to_t (v x)

abstract val toUInt64 : x:t {UInt64.fits (v x)} -> UInt64.t
abstract let toUInt64 x = UInt64.uint_to_t (v x)

abstract val toUInt32 : x:t {UInt32.fits (v x)} -> UInt32.t
abstract let toUInt32 x = UInt32.uint_to_t (v x)

abstract val tryToInt64 : x:t -> option Int64.t
abstract let tryToInt64 x = if Int64.fits (v x) then Some <| Int64.int_to_t (v x) else None

abstract val tryToUInt64 : x:t -> option UInt64.t
abstract let tryToUInt64 x = if UInt64.fits (v x) then Some <| UInt64.uint_to_t (v x) else None

abstract val tryToUInt32 : x:t -> option UInt32.t
abstract let tryToUInt32 x = if UInt32.fits (v x) then Some <| UInt32.uint_to_t (v x) else None

val v_fromInt64 : x:Int64.t {fits (Int64.v x)} -> Lemma
    (Int64.v x == v (fromInt64 x))
let v_fromInt64 x = ()

val v_fromUInt64 : x:UInt64.t {fits (UInt64.v x)} -> Lemma
    (UInt64.v x == v (fromUInt64 x))
let v_fromUInt64 x = ()

val v_fromUInt32 : x:UInt32.t {fits (UInt32.v x)} -> Lemma
    (UInt32.v x == v (fromUInt32 x))
let v_fromUInt32 x = ()

val v_toInt64 : x:t {Int64.fits (v x)} -> Lemma
    (Int64.v (toInt64 x) == v x)
let v_toInt64 x = ()

val v_toUInt64 : x:t {UInt64.fits (v x)} -> Lemma
    (UInt64.v (toUInt64 x) == v x)
let v_toUInt64 x = ()

val v_toUInt32 : x:t {UInt32.fits (v x)} -> Lemma
    (UInt32.v (toUInt32 x) == v x)
let v_toUInt32 x = ()

val v_tryFromInt64 : x:Int64.t -> Lemma
    (fits (Int64.v x) ==> Int64.v x == v (Some?.v (tryFromInt64 x)))
let v_tryFromInt64 x = ()

val v_tryFromUInt64 : x:UInt64.t -> Lemma
    (fits (UInt64.v x) ==> UInt64.v x == v (Some?.v (tryFromUInt64 x)))
let v_tryFromUInt64 x = ()

val v_tryFromUInt32 : x:UInt32.t -> Lemma
    (fits (UInt32.v x) ==> UInt32.v x == v (Some?.v (tryFromUInt32 x)))
let v_tryFromUInt32 x = ()

val v_tryToInt64 : x:t -> Lemma
    (Int64.fits (v x) ==> Int64.v (Some?.v (tryToInt64 x)) == v x)
let v_tryToInt64 x = ()

val v_tryToUInt64 : x:t -> Lemma
    (UInt64.fits (v x) ==> UInt64.v (Some?.v (tryToUInt64 x)) == v x)
let v_tryToUInt64 x = ()

val v_tryToUInt32 : x:t -> Lemma
    (UInt32.fits (v x) ==> UInt32.v (Some?.v (tryToUInt32 x)) == v x)
let v_tryToUInt32 x = ()
