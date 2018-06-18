module Zen.Hash.Base

open Zen.Cost
open Zen.Types

type t = hash

unfold val parse: string -> option hash `cost` 32
unfold let parse = Zen.Crypto.parseHash
