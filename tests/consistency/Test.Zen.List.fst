module Test.Zen.List

open Zen.Cost
open Zen.Base
open Zen.List

val test1 : list nat `cost` 8
let test1 = rev [ 1 ; 2 ; 3 ]
