#I "../.paket/load/net47"
#r "../bin/Zulib.dll"
#r "../packages/FsBech32/lib/net47/FsBech32.dll"
#load "FsCheck.fsx"
#load "FSharpx.Collections.fsx"

open FsCheck

module Cost = Zen.Cost.Realized
module ContractId = Zen.ContractId

module C = Zen.Cost.Realized
module P = Prims
module Char = FStar.Char

module Asset = Zen.Asset

type CharProperties =

    static member ``zLowercase equivalent to fsLowercase`` (c: char) =
        let charV = System.Convert.ToInt32 c
        (0 <= charV && charV <= 255) ==>
        lazy ( let zLowercase = Char.lowercase (byte charV)
                                |> C.__force
                                |> char
               let fsLowercase = System.Char.ToLower c
               zLowercase = fsLowercase )

    static member ``zUppercase equivalent to fsUppercase`` (c: char) =
        let charV = System.Convert.ToInt32 c
        (0 <= charV && charV <= 255) ==>
        lazy ( let zUppercase = Char.uppercase (byte charV)
                                |> C.__force
                                |> char
               let fsUppercase = System.Char.ToUpper c
               zUppercase = fsUppercase )
        
        
    static member ``zTest Asset`` (c: Zen.Types.Extracted.asset) =
        lazy (
                 "00"
                 |> System.Text.Encoding.ASCII.GetBytes //fsTofstString
                 |> Asset.parse
                 |> C.__force
                 |> printfn "2's 0 %A"
                 
                 "0000"
                 |> System.Text.Encoding.ASCII.GetBytes
                 |> Asset.parse
                 |> C.__force
                 |> printfn "4's 0 %A"
                 
                 "000000000000000000000000000000000000000000000000000000000000000000000000"
                 |> System.Text.Encoding.ASCII.GetBytes
                 |> Asset.parse
                 |> C.__force
                 |> printfn "72's 0 %A"
                 
                 "00000000b6b4cd1a44147f7864d3bb0af09b8cbc223f7f13c599cc00be5aa2dceeae983792b80069ab47067a11d010933e7ff6051dc75a0aeba1eaea79b445c0ea9e5a8e"
                 |> System.Text.Encoding.ASCII.GetBytes
                 |> Asset.parse
                 |> C.__force
                 |> printfn "136's cid %A"
                 
                 
                 "00000000a515de480812021d184014dc43124254ddc6b994331bc8abe5fbd6c04bc3c130"
                 |> System.Text.Encoding.ASCII.GetBytes
                 |> Asset.parse
                 |> C.__force
                 |> printfn "72's cid %A"
             ) 
        



Check.QuickAll<CharProperties>()
