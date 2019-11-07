#I "../.paket/load/net47"
#r "../bin/Zulib.dll"
#r "../packages/FsBech32/lib/net45/FsBech32.dll"
#load "FsCheck.fsx"
#load "FSharpx.Collections.fsx"

open FsCheck

module C = Zen.Cost.Realized
module P = Prims
module ZA = Zen.Asset

let _ =
    ZA.parse "00000000a515de480812021d184014dc43124254ddc6b994331bc8abe5fbd6c04bc3c130"B
    |> C.__force
    |> function
    | Some _ -> ()
    | None -> failwith "Couldn't parse short form"

let _ =
    ZA.parse "00000000c0f9c8be539cf3ba007b15d48882af748f8bfa431d1c4c7282f4650a689a83f45553440000000000000000000000000000000000000000000000000000000000"B
    |> C.__force
    |> function
    | Some _ -> ()
    | None -> failwith "Couldn't parse long form"

let _ =
    ZA.parse "1234"B
    |> C.__force
    |> function
    | Some _ -> failwith "Parsed invalid form"
    | None -> ()

let _ =
    ZA.parse "00"B
    |> C.__force
    |> function
    | Some _ -> ()
    | None -> failwith "Couldn't parse the Zen asset short form"
