module Zen.Asset

module S = Zen.String
module U32 = Zen.UInt32

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
    -> s:string { S.length s <= 32 }
    -> asset `cost` 64

val fromSubtypeInt:
    contractId
    -> U32.t
    -> asset `cost` 64
