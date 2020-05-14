module Zen.ListRealized

open Zen.Base
open Zen.Cost

val reverse (#a:Type): ls:list a -> list a `cost` (2 * length ls + 2)
