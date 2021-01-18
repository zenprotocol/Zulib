#I "../.paket/load/net47"
#r "../packages/BouncyCastle/lib/BouncyCastle.Crypto.dll"
#r "../packages/FSharp.Compatibility.OCaml/lib/net45/FSharp.Compatibility.OCaml.dll"
#r "../bin/Zulib.dll"
#load "FsCheck.fsx"
#load "FSharpx.Collections.fsx"

open FsCheck

module C = Zen.Cost.Realized
module P = Prims
module RT = Zen.ResultT
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

    static member ``list of all oks becomes ok of list`` (xs: ZL.t<int>) =
        (RT.tryMapT 0L (fun x -> RT.ok () x) xs |> C.__force) = (RT.ok () xs |> C.__force)
    
    static member ``list with an error becomes an error`` (xs: int list) (ys : int list) =
        let map =
            FSharp.Collections.List.map
        
        let ls =    
            map (RT.ok ()) xs @ [RT.failw () "ERROR"B] @ map (RT.ok ()) ys
            |> fsListToZList
        
        ((RT.tryMapT 0L (fun x -> x) ls) |> C.__force) = (RT.failw () "ERROR"B |> C.__force)
    

Check.QuickThrowOnFailureAll<TryMapTProperties>()
