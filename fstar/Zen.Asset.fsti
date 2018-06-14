module Zen.Asset

module S = FStar.String
module U32 = FStar.UInt32

open Zen.Cost
open Zen.Types

val zeroHash:hash
val zenAsset:asset

val getDefault:
    contractId
    -> asset `cost` 64

val parse:
    string
    -> option asset `cost` 64

val fromSubtypeString:
    contractId
    -> s:string { S.length s <= 29 }
    -> asset `cost` 64

val fromSubtypeInt:
    contractId
    -> U32.t
    -> asset `cost` 64
