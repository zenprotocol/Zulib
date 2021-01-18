#I "../.paket/load/net47"
#r "../packages/BouncyCastle/lib/BouncyCastle.Crypto.dll"
#r "../packages/FSharp.Compatibility.OCaml/lib/net45/FSharp.Compatibility.OCaml.dll"
#r "../packages/BouncyCastle/lib/BouncyCastle.Crypto.dll"
#r "../bin/Zulib.dll"


#load "FsCheck.fsx"
#load "FSharpx.Collections.fsx"


open FsCheck
open Org.BouncyCastle.Crypto.Digests
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



let hash1' =
    Sha3E.hashWith 0L Sha3E.updateCPK cpk1
    |> C.__force
if hash1' = hash1 then
    printf "Sha3.hashWith is working properly."
else
    failwith "Sha3.hashWith isn't working properly."






let empty = new Sha3Digest(256)

let U32ToBytes (value:uint32) =
    let bytes = System.BitConverter.GetBytes value

    if System.BitConverter.IsLittleEndian then
        Array.rev bytes
    else
        bytes

let uU32 (x : uint32) (sha3 : Sha3Digest) : Sha3Digest =
    let sha3 = Sha3Digest sha3
    sha3.BlockUpdate(U32ToBytes x, 0, 4)
    sha3

let finalize (sha3:Sha3Digest): Extracted.hash =
    let sha3 = Sha3Digest sha3
    let hash = Array.zeroCreate 32

    sha3.DoFinal(hash, 0) |> ignore
    hash

let h1 : Extracted.hash =
    empty
    |> uU32 3ul
    |> uU32 2ul
    |> uU32 1ul
    |> finalize

let ofFsList (ls : 'a list) : 'a Prims.list =
    let rec aux (ls : 'a list) (acc : 'a Prims.list) : 'a Prims.list =
        match ls with
        | [] -> acc
        | (hd::tl) -> aux tl (Prims.Cons(hd,acc))
    aux ls Prims.Nil
    |> Zen.List.rev
    |> C.__force

let ls1 =
    [ 1ul ; 2ul ; 3ul ]
    |> ofFsList

let h2 : Extracted.hash =
    ls1
    |> Sha3E.hashWith 0L (Sha3E.updateListWith 0L Sha3.updateU32)
    |> C.__force

if h1 <> h2 then
    printfn "Sha3.updateListWith is working properly."
else
    failwith "Sha3.updateListWith isn't working properly."



let arr1 =
    ls1
    |> Zen.Array.Base.ofList
    |> C.__force

let h2a : Extracted.hash =
    arr1
    |> Sha3E.hashWith 0L (Sha3E.updateArrayWith 0L Sha3.updateU32)
    |> C.__force

if h1 <> h2a then
    printfn "Sha3.updateArrayWith is working properly."
else
    failwith "Sha3.updateArrayWith isn't working properly."