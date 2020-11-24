
#I "../.paket/load/net47"
#r "../packages/BouncyCastle/lib/BouncyCastle.Crypto.dll"
#r "../bin/Zulib.dll"


#load "FsCheck.fsx"
#load "FSharpx.Collections.fsx"


open FsCheck

open Zen.Types

module P     = Prims
module C     = Zen.Cost.Realized
module P     = Prims
module Sha3  = Zen.Sha3.Realized
module Sha3E = Zen.Sha3.Extracted


// let pks1 =
//     "03f2daa1ccca6e437e01a2c943ae092e2111b65949da5b7284dc15a4a5ae82bf16"

let cpk1 =
    (3uy
    , [| 242uy; 218uy; 161uy; 204uy; 202uy; 110uy; 67uy; 126uy; 1uy; 162uy; 201uy;
       67uy; 174uy; 9uy; 46uy; 33uy; 17uy; 182uy; 89uy; 73uy; 218uy; 91uy; 114uy;
       132uy; 220uy; 21uy; 164uy; 165uy; 174uy; 130uy; 191uy; 22uy|])

// let pks2 =
//     "02a4519768d19f7a5af0937092c2e405595ddec187f62ffa85cf9902eaf0afee37"

let cpk2 =
    (2uy
    , [|164uy; 81uy; 151uy; 104uy; 209uy; 159uy; 122uy; 90uy; 240uy; 147uy; 112uy;
          146uy; 194uy; 228uy; 5uy; 89uy; 93uy; 222uy; 193uy; 135uy; 246uy; 47uy; 250uy;
          133uy; 207uy; 153uy; 2uy; 234uy; 240uy; 175uy; 238uy; 55uy|])

let hash1 =
    Sha3.empty
    |> Sha3E.updateCPK cpk1
    |> C.__force
    |> Sha3.finalize
    |> C.__force

let hash2 =
    Sha3.empty
    |> Sha3E.updateCPK cpk2
    |> C.__force
    |> Sha3.finalize
    |> C.__force

if hash1 = hash2 then
    failwith "Sha3.updateCPK doesn't update properly."
else
    printfn "Sha3.updateCPK updates properly."
