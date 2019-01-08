module Zen.UInt32.Conversion

module UInt64 = FStar.Int64
module UInt64 = FStar.UInt64
open            FStar.UInt32
module UInt8  = FStar.UInt8

open FSharp.Core.Operators.Checked

let fromInt64 (x:int64) : t = uint32 x

let fromUInt64 (x:uint64) : t = uint32 x

let fromUInt8 (x:uint8) : t = uint32 x

let fromInt64opt (x:int64) : option<t> =
    if int64 System.UInt32.MinValue <= x
        then Some <| uint32 x
        else None

let fromUInt64opt (x:uint64) : option<t> =
    if x <= uint64 System.UInt32.MaxValue
        then Some <| uint32 x
        else None

let fromUInt8opt (x:uint8) : option<t> = Some <| uint32 x

let toInt64 (x:t) : int64 = int64 x

let toUInt64 (x:t) : uint64 = uint64 x

let toUInt8 (x:t) : uint8 = uint8 x

let toInt64opt (x:t) : option<int64> = Some <| int64 x

let toUInt64opt (x:t) : option<uint64> = Some <| uint64 x

let toUInt8opt (x:t) : option<uint8> =
    if x <= uint32 System.Byte.MaxValue
        then Some <| uint8 x
        else None

let v_invariant_fromInt64 (x:int64) : unit = ()

let v_invariant_fromUInt64 (x:uint64) : unit = ()

let v_invariant_fromUInt8 (x:uint8) : unit = ()

let v_invariant_toInt64 (x:uint32) : unit = ()

let v_invariant_toUInt64 (x:uint32) : unit = ()

let v_invariant_toUInt8 (x:uint32) : unit = ()

let inbounds_some_fromInt64opt (x:int64) : unit = ()

let inbounds_some_fromUInt64opt (x:uint64) : unit = ()

let inbounds_some_fromUInt8opt (x:uint8) : unit = ()

let inbounds_some_toInt64opt (x:uint32) : unit = ()

let inbounds_some_toUInt64opt (x:uint32) : unit = ()

let inbounds_some_toUInt8opt (x:uint32) : unit = ()
