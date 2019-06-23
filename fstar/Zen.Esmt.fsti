module Zen.Esmt

open Zen.Cost
open Zen.Types

val verify:
    cwt: string
    -> defaultHash: hash
    -> root: hash
    -> auditPath: list hash
    -> key: hash
    -> value: option hash
    -> bool `cost` 111134 // (256 * (192 + 192 + 20 + 30) + 30)

val serializeU64:
    value: FStar.UInt64.t
    -> hash `cost` 48 // (6 * 8)
