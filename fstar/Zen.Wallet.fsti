module Zen.Wallet

open Zen.Types
open Zen.Option
open Zen.Cost

module U64 = Zen.UInt64

type t = wallet

val size : wallet -> nat
