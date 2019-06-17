module Zen.SparseMerkleTree

open Zen.Cost
open Zen.Types

val verify:
    cwt: string
    -> defaultHash: hash
    -> root: hash
    -> auditPath: list hash
    -> key: hash
    -> value: option hash
    -> bool `cost` (length auditPath * 420 + 4)

val serializeU64:
    value: FStar.UInt64.t
    -> hash
