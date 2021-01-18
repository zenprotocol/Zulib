#I "../.paket/load/net47"
#r "../packages/BouncyCastle/lib/BouncyCastle.Crypto.dll"
#r "../packages/FSharp.Compatibility.OCaml/lib/net45/FSharp.Compatibility.OCaml.dll"
#r "../bin/Zulib.dll"
#load "FsCheck.fsx"
#load "FSharpx.Collections.fsx"

open FsCheck

module C = Zen.Cost.Realized
module P = Prims
module OT = Zen.OptionT
module C = Zen.Cost.Realized
module P = Prims
module ZL = Zen.List


let rec fsListToZList: list<'a> -> ZL.t<'a> = function
    | [] -> P.Nil
    | hd::tl -> Prims.Cons(hd, fsListToZList tl)

let rec zListToFSList: ZL.t<'a> -> list<'a> = function
    | P.Nil -> []
    | P.Cons(hd, tl) -> hd::zListToFSList tl

let zListGen<'a> : Gen<ZL.t<'a>> =
    gen { let! ls = Arb.generate<list<'a>>
          return (fsListToZList ls) }

type Generators =
    static member ZList<'a>() =
        {new Arbitrary<ZL.t<'a>>() with
            override x.Generator = zListGen
            override x.Shrinker _ = Seq.empty}
Arb.register<Generators>()

type TryMapTProperties =

    static member ``list of all somes becomes some of list`` (xs: ZL.t<int>) =
        (OT.tryMapT 0L (fun x -> OT.some () x) xs |> C.__force) = (OT.some () xs |> C.__force)
    
    static member ``list with a none becomes none`` (xs: int list) (ys : int list) =
        let map =
            FSharp.Collections.List.map
        
        let ls =    
            map (OT.some ()) xs @ [OT.none ()] @ map (OT.some ()) ys
            |> fsListToZList
        
        ((OT.tryMapT 0L (fun x -> x) ls) |> C.__force) = (OT.none () |> C.__force)
    

Check.QuickThrowOnFailureAll<TryMapTProperties>()
