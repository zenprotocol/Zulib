module Test_Zen_Dictionary


open Zen.Base
open Zen.Cost

module D = Zen.Dictionary

type foo =
    | Foo of int

let test_add : D.dictionary int `cost` (64 + 64 + 64) =
    ret D.empty
    >>= D.add "zxcv" 5
    >>= D.add "asdf" 10
    >>= D.add "qwer" 1

let test_mapT : D.dictionary foo `cost` (D.size D.empty * (0 + 2) + 2 + 64) =
    D.empty
    |> (fun x -> D.mapT (fun _ x -> ret (Foo (x + 1))) x)
    >>= D.add "zxcv" (Foo 5)

let test_foldT : int `cost` (D.size D.empty * (0 + 4) + 4 + 0) =
    let! res = D.foldT (fun acc key value -> ret (acc + value)) 0 D.empty
    in ret (res + 5)

let test_size : nat =
    D.size (D.empty <: D.t int) + 5

let test_containsKey : int `cost` 64 =
    let! res = D.containsKey "qwer" (D.empty <: D.t int)
    in if res then ret 3 else ret 8

let test_tryFind : int `cost` (64 + 0) =
    let! res = D.tryFind "zxcv" (D.empty <: D.t int)
    in match res with | Some x -> ret (x + 5) | None -> ret 8

let test_remove : D.t int `cost` (64 + 64) =
    let! res = D.remove "zxcv" (D.empty <: D.t int)
    in D.add "qewr" 1 res
