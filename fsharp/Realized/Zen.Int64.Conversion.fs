module Zen.Int64.Conversion

open FStar.Int64
open FSharp.Core.Operators.Checked

module UInt64 = FStar.UInt64
module UInt32 = FStar.UInt32
module UInt8  = FStar.UInt8

let fromUInt64 (x:uint64) : t = int64 x

let fromUInt32 (x:uint32) : t = int64 x

let fromUInt8 (x:uint8) : t = int64 x

let tryFromUInt64 (x:uint64) : option<t> =
    if x <= uint64 System.Int64.MaxValue
        then Some <| int64 x
        else None

let tryFromUInt32 (x:uint32) : option<t> = Some <| int64 x

let tryFromUInt8 (x:uint8) : option<t> = Some <| int64 x

let toUInt64 (x:t) : uint64 = uint64 x

let toUInt32 (x:t) : uint32 = uint32 x

let toUInt8 (x:t) : uint8 = byte x

let tryToUInt64 (x:t) : option<uint64> =
    if int64 System.UInt64.MinValue <= x
        then Some <| uint64 x
        else None

let tryToUInt32 (x:t) : option<uint32> =
    if int64 System.UInt32.MinValue <= x
        then Some <| uint32 x
        else None

let tryToUInt8 (x:t) : option<uint8> =
    if int64 System.Byte.MinValue <= x && x <= int64 System.Byte.MaxValue
        then Some <| byte x
        else None

let v_fromUInt64 (x:uint64) : unit = ()

let v_fromUInt32 (x:uint32) : unit = ()

let v_fromUInt8 (x:uint8) : unit = ()

let v_toUInt64 (x:int64) : unit = ()

let v_toUInt32 (x:int64) : unit = ()

let v_toUInt8 (x:int64) : unit = ()

let v_tryFromUInt64 (x:uint64) : unit = ()

let v_tryFromUInt32 (x:uint32) : unit = ()

let v_tryFromUInt8 (x:uint8) : unit = ()

let v_tryToUInt64 (x:int64) : unit = ()

let v_tryToUInt32 (x:int64) : unit = ()

let v_tryToUInt8 (x:int64) : unit = ()
