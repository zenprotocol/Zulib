module Zen.UInt64.Conversion

module UInt64 = FStar.Int64
open            FStar.UInt64
module UInt32 = FStar.UInt32
module UInt8  = FStar.UInt8

abstract val fromInt64 : x:Int64.t {fits (Int64.v x)} -> t
abstract let fromInt64 x = uint_to_t (Int64.v x)

abstract val fromUInt32 : x:UInt32.t {fits (UInt32.v x)} -> t
abstract let fromUInt32 x = uint_to_t (UInt32.v x)

abstract val fromUInt8 : x:UInt8.t {fits (UInt8.v x)} -> t
abstract let fromUInt8 x = uint_to_t (UInt8.v x)

abstract val fromInt64opt : x:Int64.t -> option t
abstract let fromInt64opt x = if fits (Int64.v x) then Some <| uint_to_t (Int64.v x) else None

abstract val fromUInt32opt : x:UInt32.t -> option t
abstract let fromUInt32opt x = if fits (UInt32.v x) then Some <| uint_to_t (UInt32.v x) else None

abstract val fromUInt8opt : x:UInt8.t -> option t
abstract let fromUInt8opt x = if fits (UInt8.v x) then Some <| uint_to_t (UInt8.v x) else None

abstract val toInt64 : x:t {Int64.fits (v x)} -> Int64.t
abstract let toInt64 x = Int64.int_to_t (v x)

abstract val toUInt32 : x:t {UInt32.fits (v x)} -> UInt32.t
abstract let toUInt32 x = UInt32.uint_to_t (v x)

abstract val toUInt8 : x:t {UInt8.fits (v x)} -> UInt8.t
abstract let toUInt8 x = UInt8.uint_to_t (v x)

abstract val toInt64opt : x:t -> option Int64.t
abstract let toInt64opt x = if Int64.fits (v x) then Some <| Int64.int_to_t (v x) else None

abstract val toUInt32opt : x:t -> option UInt32.t
abstract let toUInt32opt x = if UInt32.fits (v x) then Some <| UInt32.uint_to_t (v x) else None

abstract val toUInt8opt : x:t -> option UInt8.t
abstract let toUInt8opt x = if UInt8.fits (v x) then Some <| UInt8.uint_to_t (v x) else None

val v_invariant_fromInt64 : x:Int64.t {fits (Int64.v x)} -> Lemma
    (ensures (Int64.v x == v (fromInt64 x)))
let v_invariant_fromInt64 x = ()

val v_invariant_fromUInt32 : x:UInt32.t {fits (UInt32.v x)} -> Lemma
    (ensures (UInt32.v x == v (fromUInt32 x)))
let v_invariant_fromUInt32 x = ()

val v_invariant_fromUInt8 : x:UInt8.t {fits (UInt8.v x)} -> Lemma
    (ensures (UInt8.v x == v (fromUInt8 x)))
let v_invariant_fromUInt8 x = ()

val v_invariant_toInt64 : x:t {Int64.fits (v x)} -> Lemma
    (ensures (Int64.v (toInt64 x) == v x))
let v_invariant_toInt64 x = ()

val v_invariant_toUInt32 : x:t {UInt32.fits (v x)} -> Lemma
    (ensures (UInt32.v (toUInt32 x) == v x))
let v_invariant_toUInt32 x = ()

val v_invariant_toUInt8 : x:t {UInt8.fits (v x)} -> Lemma
    (ensures (UInt8.v (toUInt8 x) == v x))
let v_invariant_toUInt8 x = ()

val inbounds_some_fromInt64opt : x:Int64.t {fits (Int64.v x)} -> Lemma
    (Some? (fromInt64opt x))
let inbounds_some_fromInt64opt x = ()

val inbounds_some_fromUInt32opt : x:UInt32.t {fits (UInt32.v x)} -> Lemma
    (Some? (fromUInt32opt x))
let inbounds_some_fromUInt32opt x = ()

val inbounds_some_fromUInt8opt : x:UInt8.t {fits (UInt8.v x)} -> Lemma
    (Some? (fromUInt8opt x))
let inbounds_some_fromUInt8opt x = ()

val inbounds_some_toInt64opt : x:t {Int64.fits (v x)} -> Lemma
    (Some? (toInt64opt x))
let inbounds_some_toInt64opt x = ()

val inbounds_some_toUInt32opt : x:t {UInt32.fits (v x)} -> Lemma
    (Some? (toUInt32opt x))
let inbounds_some_toUInt32opt x = ()

val inbounds_some_toUInt8opt : x:t {UInt8.fits (v x)} -> Lemma
    (Some? (toUInt8opt x))
let inbounds_some_toUInt8opt x = ()
