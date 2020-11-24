module Zen.Sha3.Extracted

open Zen.Base
open Zen.Cost

module Sha3 = Zen.Sha3.Realized
module PK = Zen.PublicKey

val updateCPK : PK.cpk -> Sha3.t -> Sha3.t `cost` 203
let updateCPK (parity, h) s =
    Sha3.updateByte parity s
    >>= Sha3.updateHash h
    |> Zen.Cost.inc 5
