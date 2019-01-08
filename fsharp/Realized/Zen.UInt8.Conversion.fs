module Zen.UInt8.Conversion

module UInt64 = FStar.Int64
module UInt64 = FStar.UInt64
module UInt32 = FStar.UInt32
open            FStar.UInt8

open FSharp.Core.Operators.Checked

let fromInt64 (x:int64) : t = byte x

let fromUInt64 (x:uint64) : t = byte x

let fromUInt32 (x:uint32) : t = byte x

let fromInt64opt (x:int64) : option<t> =
    if int64 System.Byte.MinValue <= x && x <= int64 System.Byte.MaxValue
        then Some <| byte x
        else None

let fromUInt64opt (x:uint64) : option<t> =
    if x <= uint64 System.Byte.MaxValue
        then Some <| byte x
        else None

let fromUInt32opt (x:uint32) : option<t> =
    if x <= uint32 System.Byte.MaxValue
        then Some <| byte x
        else None

let toInt64 (x:t) : int64 = int64 x

let toUInt64 (x:t) : uint64 = uint64 x

let toUInt32 (x:t) : uint32 = uint32 x

let toInt64opt (x:t) : option<int64> = Some <| int64 x

let toUInt64opt (x:t) : option<uint64> = Some <| uint64 x

let toUInt32opt (x:t) : option<uint32> = Some <| uint32 x

let v_invariant_fromInt64 (x:int64) : unit = ()

let v_invariant_fromUInt64 (x:uint64) : unit = ()

let v_invariant_fromUInt32 (x:uint32) : unit = ()

let v_invariant_toInt64 (x:byte) : unit = ()

let v_invariant_toUInt64 (x:byte) : unit = ()

let v_invariant_toUInt32 (x:byte) : unit = ()

let inbounds_some_fromInt64opt (x:int64) : unit = ()

let inbounds_some_fromUInt64opt (x:uint64) : unit = ()

let inbounds_some_fromUInt32opt (x:uint32) : unit = ()

let inbounds_some_toInt64opt (x:byte) : unit = ()

let inbounds_some_toUInt64opt (x:byte) : unit = ()

let inbounds_some_toUInt32opt (x:byte) : unit = ()
