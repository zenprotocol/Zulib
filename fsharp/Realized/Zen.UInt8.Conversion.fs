module Zen.UInt8.Conversion

open FStar.UInt8
open FSharp.Core.Operators.Checked

module UInt64 = FStar.Int64
module UInt64 = FStar.UInt64
module UInt32 = FStar.UInt32

let fromInt64 (x:uint64) : t = byte x

let fromUInt64 (x:uint64) : t = byte x

let fromUInt32 (x:uint32) : t = byte x

let tryFromInt64 (x:int64) : option<t> =
    if int64 System.Byte.MinValue <= x && x <= int64 System.Byte.MaxValue
        then Some <| byte x
        else None

let tryFromUInt64 (x:uint64) : option<t> =
    if x <= uint64 System.Byte.MaxValue
        then Some <| byte x
        else None

let tryFromUInt32 (x:uint32) : option<t> =
    if x <= uint32 System.Byte.MaxValue
        then Some <| byte x
        else None

let toInt64 (x:t) : int64 = int64 x

let toUInt64 (x:t) : uint64 = uint64 x

let toUInt32 (x:t) : uint32 = uint32 x

let tryToInt64 (x:t) : option<int64> = Some <| int64 x

let tryToUInt64 (x:t) : option<uint64> = Some <| uint64 x

let tryToUInt32 (x:t) : option<uint32> = Some <| uint32 x

let v_fromInt64 (x:int64) : unit = ()

let v_fromUInt64 (x:uint64) : unit = ()

let v_fromUInt32 (x:uint32) : unit = ()

let v_toInt64 (x:uint8) : unit = ()

let v_toUInt64 (x:uint8) : unit = ()

let v_toUInt32 (x:uint8) : unit = ()

let v_tryFromInt64 (x:int64) : unit = ()

let v_tryFromUInt64 (x:uint64) : unit = ()

let v_tryFromUInt32 (x:uint32) : unit = ()

let v_tryToInt64 (x:uint8) : unit = ()

let v_tryToUInt64 (x:uint8) : unit = ()

let v_tryToUInt32 (x:uint8) : unit = ()
