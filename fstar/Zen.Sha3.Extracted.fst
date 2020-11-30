module Zen.Sha3.Extracted

open Zen.Base
open Zen.Cost
open Zen.Types

module Sha3 = Zen.Sha3.Realized
module PK = Zen.PublicKey
module A = Zen.Array

val updateCPK: PK.cpk -> Sha3.t -> Sha3.t `cost` 205
let updateCPK (parity, h) s =
  ret s
  >>= Sha3.updateByte parity
  >>= Sha3.updateHash h
  |> inc 7

val updateListWith (#a : Type) (#n : nat) :
    (a -> Sha3.t -> Sha3.t `cost` n)
    -> ls: list a
    -> Sha3.t
    -> Sha3.t `cost` (length ls * (n + 4) + 8)
let updateListWith #_ #_ upd ls sha3 =
  Zen.List.foldT (flip upd) sha3 ls
  |> inc 4

val hashWith (#a : Type) (#n : nat) :
    (a -> Sha3.t -> Sha3.t `cost` n)
    -> a
    -> hash `cost` (n + 26)
let hashWith #_ #_ upd x =
  ret Sha3.empty
  >>= upd x
  >>= Sha3.finalize
  |> inc 6