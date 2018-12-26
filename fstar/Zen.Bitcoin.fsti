module Zen.Bitcoin

open Zen.Base
open Zen.Cost
open Zen.Types

module U8 = FStar.UInt8
module U32 = FStar.UInt32
module A = Zen.Array

type bitcoinHeader = A.indexed U8.byte 80
type target = A.indexed U8.byte 4

val difficultyAdjustmentInterval : U32.t

val parent:
    bitcoinHeader
    -> hash `cost` 4

val nbits:
    bitcoinHeader
    -> target `cost` 4

val parseHeader:
    string
    -> option bitcoinHeader `cost` 120

val computeHeaderHash:
    bitcoinHeader
    -> hash `cost` 500

val checkProofOfWork:
    hash
    -> target
    -> bool `cost` 700

val calculateNextWorkRequired:
    first:bitcoinHeader
    -> last:bitcoinHeader
    -> target `cost` 1500

val checkInclusion(#l:nat) :
    auditPath : hash `A.indexed` l
    -> index : U32.t
    -> txHash: hash
    -> header:bitcoinHeader
    -> bool `cost` ((l + 1) * 550 + 50)
