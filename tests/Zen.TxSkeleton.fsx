#I "../.paket/load/net47"
#r "../packages/BouncyCastle/lib/BouncyCastle.Crypto.dll"
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

let zp = 100_000_000UL

let asset = Zen.Asset.zenAsset

let amount = 14_000_000_000_000UL

let hash = Zen.Hash.Sha3.empty |> Zen.Hash.Sha3.finalize |> Zen.Cost.Realized.__force

let spnd = { asset=asset; amount=zp }

let out = { lock=PKLock hash; spend=spnd }

let w : wallet =
  List.init 150_000 (fun i -> { txHash=hash; index=uint32 i } , out )
  |> fsListToZList

let cid =
  Zen.ContractId.parse "00000000eac6c58bed912ff310df9f6960e8ed5c28aac83b8a98964224bab1e06c779b93"B
  |> Zen.Cost.Realized.__force
  |> Option.get


let test1 =
  TxSkel.fromWallet asset amount cid w TxSkel.emptyTxSkeleton
  |> Zen.Cost.Realized.__force
