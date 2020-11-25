module Zen.Sha3.Extracted

open Zen.Base
open Zen.Cost

module Sha3 = Zen.Sha3.Realized
module PK = Zen.PublicKey

val updateCPK: PK.cpk -> Sha3.t -> Sha3.t `cost` 205
let updateCPK (parity, h) s =
  ret s
  >>= Sha3.updateByte parity
  >>= Sha3.updateHash h
  |> Zen.Cost.inc 7

val updateListWith (#a : Type) (#n : nat) :
    (a -> Sha3.t -> Sha3.t `cost` n)
    -> ls: list a
    -> Sha3.t
    -> Sha3.t `cost` (length ls * (n + 4) + 8)
let rec updateListWith #_ #_ upd ls sha3 =
  Zen.List.foldT (flip upd) sha3 ls
  |> Zen.Cost.inc 4
