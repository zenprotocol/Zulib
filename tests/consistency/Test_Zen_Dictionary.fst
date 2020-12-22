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
