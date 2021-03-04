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

let test_updateHash : hash `cost` (6 + 20 + 6 + 20 + 32 * 6 + 32 * 6 + 20) =
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

let test_updateString : hash `cost` (6 * 4 + 6 * 4 + 20) =
  ret Sha3.empty
  >>= Sha3.updateString "ABCD"
  >>= Sha3.updateString "ABCD"
  >>= Sha3.finalize

let tets_updateSignature s =
  ret Sha3.empty
  >>= Sha3.updateSignature s
  >>= Sha3.updateSignature s
  >>= Sha3.finalize

let test_updatePublicKey : hash `cost` (120 + 64 * 6 + 64 * 6 + 20) = 
  let! opk = Zen.Crypto.parsePublicKey "02b43a1cb4cb6472e1fcd71b237eb9c1378335cd200dd07536594348d9e450967e" in
  match opk  with
  | Some pk ->
      ret Sha3.empty
      >>= Sha3.updatePublicKey pk
      >>= Sha3.updatePublicKey pk
      >>= Sha3.finalize
  | None ->
      incRet (64 * 6 + 64 * 6) Sha3.empty
      >>= Sha3.finalize

let test_updateAsset : hash `cost` (64 * 6 + 64 * 6 + 20) =
  ret Sha3.empty
  >>= Sha3.updateAsset Zen.Asset.zenAsset
  >>= Sha3.updateAsset Zen.Asset.zenAsset
  >>= Sha3.finalize

let test_updateByteArray : hash `cost` (3 * 2 + 2 + 6 * 3 + 6 * 3 + 20) =
  let! arr = A.ofList [ 1uy ; 2uy ; 3uy ] in
  ret Sha3.empty
  >>= Sha3.updateByteArray arr
  >>= Sha3.updateByteArray arr
  >>= Sha3.finalize

let test_updateOutpoint : hash `cost` (6 + 20 + 36 * 6 + 36 * 6 + 20) =
  let! h1 = 
      ret Sha3.empty
      >>= Sha3.updateByte 5uy
      >>= Sha3.finalize
  in
  let outpt = { txHash = h1 ; index = 0ul } in
  ret Sha3.empty
  >>= Sha3.updateOutpoint outpt
  >>= Sha3.updateOutpoint outpt
  >>= Sha3.finalize
