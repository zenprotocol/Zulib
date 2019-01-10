module Zen.UInt32.Conversion

open FStar.UInt32
open FSharp.Core.Operators.Checked

module Int64  = FStar.Int64
module UInt64 = FStar.UInt64
module UInt8  = FStar.UInt8

let fromInt64 (x:uint64) : t = uint32 x

let fromUInt64 (x:uint64) : t = uint32 x

let fromUInt8 (x:uint8) : t = uint32 x

let tryFromInt64 (x:int64) : option<t> =
    if int64 System.UInt32.MinValue <= x
        then Some <| uint32 x
        else None

let tryFromUInt64 (x:uint64) : option<t> =
    if x <= uint64 System.UInt32.MaxValue
        then Some <| uint32 x
        else None

let tryFromUInt8 (x:uint8) : option<t> = Some <| uint32 x

let toInt64 (x:t) : int64 = int64 x

let toUInt64 (x:t) : uint64 = uint64 x

let toUInt8 (x:t) : uint8 = byte x

let tryToInt64 (x:t) : option<int64> = Some <| int64 x

let tryToUInt64 (x:t) : option<uint64> = Some <| uint64 x

let tryToUInt8 (x:t) : option<uint8> =
    if x <= uint32 System.Byte.MaxValue
        then Some <| byte x
        else None

let v_fromInt64 (x:int64) : unit = ()

let v_fromUInt64 (x:uint64) : unit = ()

let v_fromUInt8 (x:uint8) : unit = ()

let v_toInt64 (x:uint32) : unit = ()

let v_toUInt64 (x:uint32) : unit = ()

let v_toUInt8 (x:uint32) : unit = ()

let v_tryFromInt64 (x:int64) : unit = ()

let v_tryFromUInt64 (x:uint64) : unit = ()

let v_tryFromUInt8 (x:uint8) : unit = ()

let v_tryToInt64 (x:uint32) : unit = ()

let v_tryToUInt64 (x:uint32) : unit = ()

let v_tryToUInt8 (x:uint32) : unit = ()
