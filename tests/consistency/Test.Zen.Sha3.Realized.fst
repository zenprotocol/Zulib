module Test.Zen.Sha3.Realized

open Zen.Base
open Zen.Cost
open Zen.Types

module A = Zen.Array
module I64 = FStar.Int64
module U8  = FStar.UInt8
module U32 = FStar.UInt32
module U64 = FStar.UInt64

module Sha3 = Zen.Sha3.Realized


let test_updateI64 : hash `cost` (6 * 8 + 6 * 8 + 20) =
    ret Sha3.empty
    >>= Sha3.updateI64 1L
    >>= Sha3.updateI64 2L
    >>= Sha3.finalize

let test_updateU64 : hash `cost` (6 * 8 + 6 * 8 + 20) =
    ret Sha3.empty
    >>= Sha3.updateU64 3UL
    >>= Sha3.updateU64 4UL
    >>= Sha3.finalize

let test_updateU32 : hash `cost` (6 * 4 + 6 * 4 + 20) =
    ret Sha3.empty
    >>= Sha3.updateU32 3ul
    >>= Sha3.updateU32 4ul
    >>= Sha3.finalize

let test_updateByte : hash `cost` (6 + 6 + 20) =
    ret Sha3.empty
    >>= Sha3.updateByte 5uy
    >>= Sha3.updateByte 6uy
    >>= Sha3.finalize

val test_updateHash : hash `cost` (6 + 20 + 6 + 20 + 32 * 6 + 32 * 6 + 20) =
    let! h1 = 
      ret Sha3.empty
      >>= Sha3.updateByte 5uy
      >>= Sha3.finalize
    in
    let! h2 =
      ret Sha3.empty
      >>= Sha3.updateByte 6uy
      >>= Sha3.finalize
    in
    ret Sha3.empty
    >>= Sha3.updateHash h1
    >>= Sha3.updateHash h2
    >>= Sha3.finalize
  

//hash ->
//t ->
//t `cost` (32 * 6)

(*)

val test_updateByteArray(#n:nat):
  U8.t `A.indexed` n ->
  t ->
  t `cost` (6 * n)

val test_updateString:
  s:string ->
  t ->
  t `cost` (6 * FStar.String.length s)


val test_updateSignature:
  hash ->
  t ->
  t `cost` (64 * 6)

val test_updatePublicKey:
  hash ->
  t ->
  t `cost` (64 * 6)

val test_updateAsset:
  asset ->
  t ->
  t `cost` (64 * 6)

val test_updateOutpoint:
  outpoint ->
  t ->
  t `cost` (36 * 6)

(*)
val updateByte:
  U8.t ->
  t ->
  t `cost` 6

val updateByteArray(#n:nat):
  U8.t `A.indexed` n ->
  t ->
  t `cost` (6 * n)

val updateU32:
  U32.t ->
  t ->
  t `cost` (6 * 4)

val updateU64:
  U64.t ->
  t ->
  t `cost` (6 * 8)

val updateString:
  s:string ->
  t ->
  t `cost` (6 * FStar.String.length s)

val updateHash:
  hash ->
  t ->
  t `cost` (32 * 6)

val updateSignature:
  hash ->
  t ->
  t `cost` (64 * 6)

val updatePublicKey:
  hash ->
  t ->
  t `cost` (64 * 6)

val updateAsset:
  asset ->
  t ->
  t `cost` (64 * 6)

val updateOutpoint:
  outpoint ->
  t ->
  t `cost` (36 * 6)

val finalize:
  t ->
  hash `cost` 20
