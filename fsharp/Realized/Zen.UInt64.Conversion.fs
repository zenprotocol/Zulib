module Zen.UInt64.Conversion

open FStar.UInt64
open FSharp.Core.Operators.Checked

module Int64  = FStar.Int64
module UInt32 = FStar.UInt32
module UInt8  = FStar.UInt8

let fromInt64 (x:uint64) : t = uint64 x

let fromUInt32 (x:uint32) : t = uint64 x

let fromUInt8 (x:uint8) : t = uint64 x

let tryFromInt64 (x:int64) : option<t> =
    if int64 System.UInt64.MinValue <= x
        then Some <| uint64 x
        else None

let tryFromUInt32 (x:uint32) : option<t> = Some <| uint64 x

let tryFromUInt8 (x:uint8) : option<t> = Some <| uint64 x

let toInt64 (x:t) : int64 = int64 x

let toUInt32 (x:t) : uint32 = uint32 x

let toUInt8 (x:t) : uint8 = byte x

let tryToInt64 (x:t) : option<int64> =
    if x <= uint64 System.Int64.MaxValue
        then Some <| int64 x
        else None

let tryToUInt32 (x:t) : option<uint32> =
    if x <= uint64 System.UInt32.MaxValue
        then Some <| uint32 x
        else None

let tryToUInt8 (x:t) : option<uint8> =
    if x <= uint64 System.Byte.MaxValue
        then Some <| byte x
        else None

let v_fromInt64 (x:int64) : unit = ()

let v_fromUInt32 (x:uint32) : unit = ()

let v_fromUInt8 (x:uint8) : unit = ()

let v_toInt64 (x:uint64) : unit = ()

let v_toUInt32 (x:uint64) : unit = ()

let v_toUInt8 (x:uint64) : unit = ()

let v_tryFromInt64 (x:int64) : unit = ()

let v_tryFromUInt32 (x:uint32) : unit = ()

let v_tryFromUInt8 (x:uint8) : unit = ()

let v_tryToInt64 (x:uint64) : unit = ()

let v_tryToUInt32 (x:uint64) : unit = ()

let v_tryToUInt8 (x:uint64) : unit = ()
