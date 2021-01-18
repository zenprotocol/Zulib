#I "../.paket/load/net47"
#r "../packages/BouncyCastle/lib/BouncyCastle.Crypto.dll"
#r "../packages/FSharp.Compatibility.OCaml/lib/net45/FSharp.Compatibility.OCaml.dll"
#r "../bin/Zulib.dll"

open System
open Zen.Types.Extracted
open Zen.Types.Realized

module C = Zen.Cost.Realized
module P = Prims
module Asset = Zen.Asset



let test_asset_parse_zen =
    
    Asset.parse "00"B
    |> C.__force
    |> Option.get
    |> fun asset ->
        if asset <> Asset.zenAsset then
            failwith "parsed asset wasn't Zen"
    
    Asset.parse "0000000000000000000000000000000000000000"B
    |> C.__force
    |> Option.get
    |> fun asset ->
        if asset <> Asset.zenAsset then
            failwith "parsed asset wasn't Zen"

