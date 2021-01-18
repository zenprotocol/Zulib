#I "../.paket/load/net47"
#r "../packages/BouncyCastle/lib/BouncyCastle.Crypto.dll"
#r "../packages/FSharp.Compatibility.OCaml/lib/net45/FSharp.Compatibility.OCaml.dll"
#r "../bin/Zulib.dll"
#load "FsCheck.fsx"
#load "FSharpx.Collections.fsx"

open FsCheck

module C = Zen.Cost.Realized
module P = Prims
module Char = FStar.Char


type CharProperties =

    static member ``zLowercase equivalent to fsLowercase`` (c: char) =
        let charV = System.Convert.ToInt32 c
        lazy ( let zLowercase = Char.lowercase (byte charV)
                                |> C.__force
                                |> char
               let fsLowercase = System.Char.ToLower c
               zLowercase = fsLowercase )

    static member ``zUppercase equivalent to fsUppercase`` (c: char) =
        let charV = System.Convert.ToInt32 c
        lazy ( let zUppercase = Char.uppercase (byte charV)
                                |> C.__force
                                |> char
               let fsUppercase = System.Char.ToUpper c
               zUppercase = fsUppercase )

Check.QuickThrowOnFailureAll<CharProperties>()
