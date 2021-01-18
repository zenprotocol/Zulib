#I "../.paket/load/net47"
#r "../packages/BouncyCastle/lib/BouncyCastle.Crypto.dll"
#r "../packages/FSharp.Compatibility.OCaml/lib/net45/FSharp.Compatibility.OCaml.dll"
#r "../bin/Zulib.dll"


#load "FsCheck.fsx"
#load "FSharpx.Collections.fsx"


open FsCheck

open Zen.Types

module P  = Prims
module C  = Zen.Cost.Realized
module P  = Prims
module PK = Zen.PublicKey

// let pk_string =
//     "02b43a1cb4cb6472e1fcd71b237eb9c1378335cd200dd07536594348d9e450967e"

let pk =
    [|126uy; 150uy; 80uy; 228uy; 217uy; 72uy; 67uy; 89uy; 54uy; 117uy; 208uy; 13uy;
    32uy; 205uy; 53uy; 131uy; 55uy; 193uy; 185uy; 126uy; 35uy; 27uy; 215uy;
    252uy; 225uy; 114uy; 100uy; 203uy; 180uy; 28uy; 58uy; 180uy; 238uy; 152uy;
    56uy; 124uy; 190uy; 168uy; 169uy; 226uy; 84uy; 91uy; 43uy; 58uy; 7uy; 66uy;
    13uy; 195uy; 217uy; 146uy; 110uy; 224uy; 72uy; 77uy; 25uy; 186uy; 189uy;
    232uy; 85uy; 209uy; 172uy; 204uy; 1uy; 88uy|]

let cpk =
    PK.compress pk
    |> C.__force

let expected_cpk =
    (2uy , [|180uy; 58uy; 28uy; 180uy; 203uy; 100uy; 114uy; 225uy; 252uy; 215uy; 27uy;
      35uy; 126uy; 185uy; 193uy; 55uy; 131uy; 53uy; 205uy; 32uy; 13uy; 208uy; 117uy;
      54uy; 89uy; 67uy; 72uy; 217uy; 228uy; 80uy; 150uy; 126uy|])

if cpk = expected_cpk then
    printfn "Zen.PublicKey.compress is working as expected."
else
    failwith "Zen.PublicKey.compress failed."