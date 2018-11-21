module Zen.ContractId

module U32 = Zen.UInt32
module S = Zen.String

open Zen.Cost
open Zen.Types

val parse:
    s:string { S.length s = 72 }
    -> option contractId `cost` 64
