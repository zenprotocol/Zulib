module Zen.Int64.Conversion

open            FStar.Int64
module UInt64 = FStar.UInt64
module UInt32 = FStar.UInt32
module UInt8  = FStar.UInt8

abstract val fromUInt64 : x:UInt64.t {fits (UInt64.v x)} -> t
abstract let fromUInt64 x = int_to_t (UInt64.v x)

abstract val fromUInt32 : x:UInt32.t {fits (UInt32.v x)} -> t
abstract let fromUInt32 x = int_to_t (UInt32.v x)

abstract val fromUInt8 : x:UInt8.t {fits (UInt8.v x)} -> t
abstract let fromUInt8 x = int_to_t (UInt8.v x)

abstract val fromUInt64opt : x:UInt64.t -> option t
abstract let fromUInt64opt x = if fits (UInt64.v x) then Some <| int_to_t (UInt64.v x) else None

abstract val fromUInt32opt : x:UInt32.t -> option t
abstract let fromUInt32opt x = if fits (UInt32.v x) then Some <| int_to_t (UInt32.v x) else None

abstract val fromUInt8opt : x:UInt8.t -> option t
abstract let fromUInt8opt x = if fits (UInt8.v x) then Some <| int_to_t (UInt8.v x) else None

abstract val toUInt64 : x:t {UInt64.fits (v x)} -> UInt64.t
abstract let toUInt64 x = UInt64.uint_to_t (v x)

abstract val toUInt32 : x:t {UInt32.fits (v x)} -> UInt32.t
abstract let toUInt32 x = UInt32.uint_to_t (v x)

abstract val toUInt8 : x:t {UInt8.fits (v x)} -> UInt8.t
abstract let toUInt8 x = UInt8.uint_to_t (v x)

abstract val toUInt64opt : x:t -> option UInt64.t
abstract let toUInt64opt x = if UInt64.fits (v x) then Some <| UInt64.uint_to_t (v x) else None

abstract val toUInt32opt : x:t -> option UInt32.t
abstract let toUInt32opt x = if UInt32.fits (v x) then Some <| UInt32.uint_to_t (v x) else None

abstract val toUInt8opt : x:t -> option UInt8.t
abstract let toUInt8opt x = if UInt8.fits (v x) then Some <| UInt8.uint_to_t (v x) else None

val v_invariant_fromUInt64 : x:UInt64.t {fits (UInt64.v x)} -> Lemma
    (ensures (UInt64.v x == v (fromUInt64 x)))
let v_invariant_fromUInt64 x = ()

val v_invariant_fromUInt32 : x:UInt32.t {fits (UInt32.v x)} -> Lemma
    (ensures (UInt32.v x == v (fromUInt32 x)))
let v_invariant_fromUInt32 x = ()

val v_invariant_fromUInt8 : x:UInt8.t {fits (UInt8.v x)} -> Lemma
    (ensures (UInt8.v x == v (fromUInt8 x)))
let v_invariant_fromUInt8 x = ()

val v_invariant_toUInt64 : x:t {UInt64.fits (v x)} -> Lemma
    (ensures (UInt64.v (toUInt64 x) == v x))
let v_invariant_toUInt64 x = ()

val v_invariant_toUInt32 : x:t {UInt32.fits (v x)} -> Lemma
    (ensures (UInt32.v (toUInt32 x) == v x))
let v_invariant_toUInt32 x = ()

val v_invariant_toUInt8 : x:t {UInt8.fits (v x)} -> Lemma
    (ensures (UInt8.v (toUInt8 x) == v x))
let v_invariant_toUInt8 x = ()

val inbounds_some_fromUInt64opt : x:UInt64.t {fits (UInt64.v x)} -> Lemma
    (Some? (fromUInt64opt x))
let inbounds_some_fromUInt64opt x = ()

val inbounds_some_fromUInt32opt : x:UInt32.t {fits (UInt32.v x)} -> Lemma
    (Some? (fromUInt32opt x))
let inbounds_some_fromUInt32opt x = ()

val inbounds_some_fromUInt8opt : x:UInt8.t {fits (UInt8.v x)} -> Lemma
    (Some? (fromUInt8opt x))
let inbounds_some_fromUInt8opt x = ()

val inbounds_some_toUInt64opt : x:t {UInt64.fits (v x)} -> Lemma
    (Some? (toUInt64opt x))
let inbounds_some_toUInt64opt x = ()

val inbounds_some_toUInt32opt : x:t {UInt32.fits (v x)} -> Lemma
    (Some? (toUInt32opt x))
let inbounds_some_toUInt32opt x = ()

val inbounds_some_toUInt8opt : x:t {UInt8.fits (v x)} -> Lemma
    (Some? (toUInt8opt x))
let inbounds_some_toUInt8opt x = ()
