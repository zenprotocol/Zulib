module Zen.Int64.Conversion

open FStar.Int64

module UInt64 = FStar.UInt64
module UInt32 = FStar.UInt32
module UInt8  = FStar.UInt8

abstract val fromUInt64 : x:UInt64.t {fits (UInt64.v x)} -> t
abstract let fromUInt64 x = int_to_t (UInt64.v x)

abstract val fromUInt32 : x:UInt32.t {fits (UInt32.v x)} -> t
abstract let fromUInt32 x = int_to_t (UInt32.v x)

abstract val fromUInt8 : x:UInt8.t {fits (UInt8.v x)} -> t
abstract let fromUInt8 x = int_to_t (UInt8.v x)

abstract val tryFromUInt64 : x:UInt64.t -> option t
abstract let tryFromUInt64 x = if fits (UInt64.v x) then Some <| int_to_t (UInt64.v x) else None

abstract val tryFromUInt32 : x:UInt32.t -> option t
abstract let tryFromUInt32 x = if fits (UInt32.v x) then Some <| int_to_t (UInt32.v x) else None

abstract val tryFromUInt8 : x:UInt8.t -> option t
abstract let tryFromUInt8 x = if fits (UInt8.v x) then Some <| int_to_t (UInt8.v x) else None

abstract val toUInt64 : x:t {UInt64.fits (v x)} -> UInt64.t
abstract let toUInt64 x = UInt64.uint_to_t (v x)

abstract val toUInt32 : x:t {UInt32.fits (v x)} -> UInt32.t
abstract let toUInt32 x = UInt32.uint_to_t (v x)

abstract val toUInt8 : x:t {UInt8.fits (v x)} -> UInt8.t
abstract let toUInt8 x = UInt8.uint_to_t (v x)

abstract val tryToUInt64 : x:t -> option UInt64.t
abstract let tryToUInt64 x = if UInt64.fits (v x) then Some <| UInt64.uint_to_t (v x) else None

abstract val tryToUInt32 : x:t -> option UInt32.t
abstract let tryToUInt32 x = if UInt32.fits (v x) then Some <| UInt32.uint_to_t (v x) else None

abstract val tryToUInt8 : x:t -> option UInt8.t
abstract let tryToUInt8 x = if UInt8.fits (v x) then Some <| UInt8.uint_to_t (v x) else None

val v_fromUInt64 : x:UInt64.t {fits (UInt64.v x)} -> Lemma
    (UInt64.v x == v (fromUInt64 x))
let v_fromUInt64 x = ()

val v_fromUInt32 : x:UInt32.t {fits (UInt32.v x)} -> Lemma
    (UInt32.v x == v (fromUInt32 x))
let v_fromUInt32 x = ()

val v_fromUInt8 : x:UInt8.t {fits (UInt8.v x)} -> Lemma
    (UInt8.v x == v (fromUInt8 x))
let v_fromUInt8 x = ()

val v_toUInt64 : x:t {UInt64.fits (v x)} -> Lemma
    (UInt64.v (toUInt64 x) == v x)
let v_toUInt64 x = ()

val v_toUInt32 : x:t {UInt32.fits (v x)} -> Lemma
    (UInt32.v (toUInt32 x) == v x)
let v_toUInt32 x = ()

val v_toUInt8 : x:t {UInt8.fits (v x)} -> Lemma
    (UInt8.v (toUInt8 x) == v x)
let v_toUInt8 x = ()

val v_tryFromUInt64 : x:UInt64.t -> Lemma
    (fits (UInt64.v x) ==> UInt64.v x == v (Some?.v (tryFromUInt64 x)))
let v_tryFromUInt64 x = ()

val v_tryFromUInt32 : x:UInt32.t -> Lemma
    (fits (UInt32.v x) ==> UInt32.v x == v (Some?.v (tryFromUInt32 x)))
let v_tryFromUInt32 x = ()

val v_tryFromUInt8 : x:UInt8.t -> Lemma
    (fits (UInt8.v x) ==> UInt8.v x == v (Some?.v (tryFromUInt8 x)))
let v_tryFromUInt8 x = ()

val v_tryToUInt64 : x:t -> Lemma
    (UInt64.fits (v x) ==> UInt64.v (Some?.v (tryToUInt64 x)) == v x)
let v_tryToUInt64 x = ()

val v_tryToUInt32 : x:t -> Lemma
    (UInt32.fits (v x) ==> UInt32.v (Some?.v (tryToUInt32 x)) == v x)
let v_tryToUInt32 x = ()

val v_tryToUInt8 : x:t -> Lemma
    (UInt8.fits (v x) ==> UInt8.v (Some?.v (tryToUInt8 x)) == v x)
let v_tryToUInt8 x = ()
