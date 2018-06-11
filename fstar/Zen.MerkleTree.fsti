module Zen.MerkleTree

open Zen.Cost
open Zen.Types

val verify:
    root: hash
    -> auditPath: list hash
    -> index: nat
    -> h: hash
    -> hash `cost` (length auditPath * 420 + 4)
