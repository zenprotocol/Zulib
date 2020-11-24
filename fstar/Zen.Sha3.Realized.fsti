module Zen.Sha3.Realized

open Zen.Base
open Zen.Cost
open Zen.Types

module A = Zen.Array
module I64 = FStar.Int64
module U8  = FStar.UInt8
module U32 = FStar.UInt32
module U64 = FStar.UInt64

assume type t

val empty:t

val updateI64:
  I64.t ->
  t ->
  t `cost` (6 * 8)

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
