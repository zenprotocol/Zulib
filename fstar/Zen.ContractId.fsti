module Zen.ContractId

module U32 = FStar.UInt32
module S = FStar.String

open Zen.Cost
open Zen.Types

val parse:
    s:string { S.length s = 72 }
    -> option contractId `cost` 64
