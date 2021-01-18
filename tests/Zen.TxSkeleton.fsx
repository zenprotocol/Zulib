#I "../.paket/load/net47"
#r "../packages/BouncyCastle/lib/BouncyCastle.Crypto.dll"
#r "../packages/FSharp.Compatibility.OCaml/lib/net45/FSharp.Compatibility.OCaml.dll"
#r "../bin/Zulib.dll"

open System
open Zen.Types.Extracted
open Zen.Types.Realized

module C = Zen.Cost.Realized
module P = Prims
module TxSkel = Zen.TxSkeleton
module Checked = Operators.Checked
module ZL = Zen.List

let rec fsListToZListAux (orig : list<'a>) (acc : ZL.t<'a>) : ZL.t<'a> =
    match orig with
    | [] -> acc
    | hd::tl -> fsListToZListAux tl (Prims.Cons(hd, acc))

let fsListToZList xs = fsListToZListAux xs Prims.Nil

let zp = 100000000UL

let asset : asset = Zen.Asset.zenAsset

let amount = 14000000000000UL

let hash = Zen.Hash.Sha3.empty |> Zen.Hash.Sha3.finalize |> Zen.Cost.Realized.__force

let spnd = { asset=asset; amount=zp }

let out = { lock=PKLock hash; spend=spnd }

let small_wallet : wallet =
  List.init 15 (fun i -> { txHash=hash; index=uint32 i } , out )
  |> fsListToZList

let cid =
  Zen.ContractId.parse "00000000eac6c58bed912ff310df9f6960e8ed5c28aac83b8a98964224bab1e06c779b93"B
  |> Zen.Cost.Realized.__force
  |> Option.get

let test1 =
  TxSkel.fromWallet asset amount cid small_wallet TxSkel.emptyTxSkeleton
  |> Zen.Cost.Realized.__force

let testGetAvailableTokens =
  TxSkel.emptyTxSkeleton
  |> TxSkel.addOutput out
  |> Zen.Cost.Realized.__force
  |> TxSkel.getAvailableTokens asset
  |> Zen.Cost.Realized.__force

let test_safe_mint_None : unit =
    let res =
      TxSkel.safeMint 0UL asset TxSkel.emptyTxSkeleton
      |> Zen.Cost.Realized.__force
    if res <> None then
      failwith "safe mint allows zero amount"

let test_safe_mint_Some : unit =
    let res =
      TxSkel.safeMint 123UL asset TxSkel.emptyTxSkeleton
      |> Zen.Cost.Realized.__force
    match res with
    | None ->
      failwith "safe mint allows non-zero amounts"
    | Some _ ->
      ()
