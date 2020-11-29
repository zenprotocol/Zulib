#I "../.paket/load/net47"
#r "../bin/Zulib.dll"
#load "FsCheck.fsx"
#load "FSharpx.Collections.fsx"

open FsCheck

module C = Zen.Cost.Realized
module P = Prims
module ZL = Zen.List
module FSL = Microsoft.FSharp.Collections.List
module FSL' = FSharpx.Collections.List

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
type ListProperties =

    static member ``list is head cons tail`` (xs: ZL.t<int>) =
        P.length xs <> 0L ==> lazy (xs = ZL.cons (ZL.head xs) (ZL.tail xs))

    static member ``length of tail is pred length`` (xs: ZL.t<int>) =
        P.length xs <> 0L ==> lazy (P.length (ZL.tail xs) = P.length xs - 1L)

    static member ``length of cons is succ length`` (x: int) (xs: ZL.t<int>) =
        P.length (ZL.cons x xs) = P.length xs + 1L

    static member ``length nonzero ==> is_cons`` (xs: ZL.t<int>) =
        P.length xs <> 0L ==> P.uu___is_Cons xs

    static member ``length nonzero ==> not (is_nil)`` (xs: ZL.t<int>) =
        P.length xs <> 0L ==> not (P.uu___is_Nil xs)

    static member ``is_nil <> is_cons`` (xs: ZL.t<int>) =
        P.uu___is_Nil xs <> P.uu___is_Cons xs

    static member ``zAppend equivalent to fsAppend``
        (zs: ZL.t<int>)
        (ls: list<int>) =
        let fsAppend = (zListToFSList zs)@ls
        let zappend = ZL.append zs (fsListToZList ls)
                      |> C.__force
                      |> zListToFSList
        fsAppend = zappend

    static member ``zmap equivalent to fsMap`` (zs: ZL.t<int>) =
        let zMapped  = ZL.map (~~~) zs
        let fsMapped = List.map (~~~) (zListToFSList zs)
        C.__force zMapped = fsListToZList fsMapped

    static member ``zRevAppend equivalent to fsRevAppend``
        (zs: ZL.t<int>)
        (ls: list<int>) =
        let zRevAppended = ZL.revAppend zs (fsListToZList ls)
        let fsRevAppended = List.rev (zListToFSList zs) @ ls
        C.__force zRevAppended = fsListToZList fsRevAppended

    static member ``zRev equivalent to fsRev`` (zs: ZL.t<int>) =
        let zRev  = ZL.rev zs
        let fsRev = List.rev (zListToFSList zs)
        C.__force zRev = fsListToZList fsRev

    static member ``zIntersperse equivalent to fsIntersperse``
        (z: int)
        (zs: ZL.t<int>) =
        let zIntersperse  = ZL.intersperse z zs
        let fsIntersperse =
            zListToFSList zs
            |> FSharpx.Collections.Seq.intersperse z
            |> List.ofSeq
        C.__force zIntersperse = fsListToZList fsIntersperse

    static member ``zFold equivalent to fsFold`` (z: int) (zs: ZL.t<int>) =
        let zFold = ZL.fold (*) z zs
        let fsFold = List.fold (*) z (zListToFSList zs)
        C.__force zFold = fsFold

    static member ``zSumBy equivalent to fsSumBy`` (zs: ZL.t<int64>) =
        let zSumBy = ZL.sumBy (fun z -> z % 23L) zs
        let fsSumBy = List.sumBy (fun z -> z % 23L) (zListToFSList zs)
        C.__force zSumBy = fsSumBy

    static member ``zSum equivalent to fsSum`` (zs: ZL.t<int64>) =
        let zSum = ZL.sum zs
        let fsSum = List.sum (zListToFSList zs)
        C.__force zSum = fsSum

    static member ``zMax equivalent to fsMax`` (zs: ZL.t<int64>) =
        P.length zs <> 0L ==>
        lazy ( let zMax = ZL.max zs
               let fsMax = List.max (zListToFSList zs)
               C.__force zMax = fsMax )

    static member ``zNth equivalent to fsNth`` (n': FsCheck.NonNegativeInt) (zs: ZL.t<int>) =
        lazy  (
            let zslen = P.length zs
            if zslen = 0L then
                true
            else
                let n = int64 n'.Get % zslen
                let zNth = ZL.nth zs n
                let fsNth = List.nth (zListToFSList zs) (int n)
                C.__force zNth = fsNth
        )

    static member ``zTryNth equivalent to fsTryNth`` (n': FsCheck.NonNegativeInt) (zs: ZL.t<int>) =
        lazy (
            let n = n'.Get
            let zTryNth = ZL.tryNth zs (int64 n)
            let fsTryNth = zListToFSList zs |> FSharpx.Collections.Seq.tryNth n
            match (C.__force zTryNth, fsTryNth) with
            | FStar.Pervasives.Native.Some x, Some y -> x = y
            | FStar.Pervasives.Native.None, None -> true
            | _ -> false
        )

    static member ``take length`` (npos : FsCheck.NonNegativeInt) (xs: ZL.t<int>) =
        lazy begin
            
            let k =
                if ZL.isNull () xs then
                    0L
                else
                    int64 npos.Get % P.length xs
            
            let ys =
                ZL.take k xs
                |> C.__force
            
            P.length ys = k
        end
    
    static member ``drop length`` (npos : FsCheck.NonNegativeInt) (xs: ZL.t<int>) =
        lazy begin
            
            let k =
                if ZL.isNull () xs then
                    0L
                else
                    int64 npos.Get % P.length xs
            
            let ys =
                ZL.drop k xs
                |> C.__force
            
            P.length ys = P.length xs - k
        end

    static member ``take and drop`` (npos : FsCheck.NonNegativeInt) (xs: ZL.t<int>) =
        lazy begin
            
            let k =
                if ZL.isNull () xs then
                    0L
                else
                    int64 npos.Get % P.length xs
            
            let ys =
                xs
                |> ZL.take k
                |> C.__force
                |> ZL.drop k
                |> C.__force
            
            ZL.isNull () ys
        end

    static member ``drop and take`` (npos : FsCheck.NonNegativeInt) (xs: ZL.t<int>) =
        lazy begin
            
            let k =
                if ZL.isNull () xs then
                    0L
                else
                    int64 npos.Get % P.length xs
            
            let ys =
                xs
                |> ZL.drop k
                |> C.__force
            
            let zs =
                ys
                |> ZL.take (P.length xs - k)
                |> C.__force
            
            ys = zs
        end

    static member ``zTake equiv to fsTake`` (npos : FsCheck.NonNegativeInt  ) (xs: ZL.t<int>) =
        lazy begin
            
            let k =
                if ZL.isNull () xs then
                    0L
                else
                    int64 npos.Get % P.length xs
            
            let ys =
                xs
                |> ZL.take k
                |> C.__force
              
            zListToFSList ys = FSL.take (int k) (zListToFSList xs)
        end

    static member ``unzip zip isomorphism`` (xs' : ZL.t<int>) (ys': ZL.t<int>) =
        lazy begin
            
            let n =
                min (P.length xs') (P.length ys')
            
            let xs =
                ZL.take n xs' |> C.__force

            let ys =
                ZL.take n ys' |> C.__force

            let zs =
                ZL.zip xs ys
                |> C.__force
            
            let us =
                ZL.unzip zs
                |> C.__force

            us = (xs , ys)
        end

    static member ``zip unzip isomorphism`` (ps : ZL.t<int * int>) =
        lazy begin
            
            let (xs , ys) =
                ZL.unzip ps
                |> C.__force

            let zs =
                ZL.zip xs ys
                |> C.__force
            
            zs = ps
        end

    static member ``zipWithT pair equiv zip`` (xs' : ZL.t<int>) (ys': ZL.t<int>) =
        lazy begin
            
            let n =
                min (P.length xs') (P.length ys')
            
            let xs =
                ZL.take n xs' |> C.__force

            let ys =
                ZL.take n ys' |> C.__force

            let zws =
                ZL.zipWithT 0L (fun x y -> C.ret (x , y)) xs ys
                |> C.__force

            let zs =
                ZL.zip xs ys
                |> C.__force
            
            zws = zs
        end

    static member ``zipWithT addition`` (xs' : ZL.t<int>) (ys': ZL.t<int>) =
        lazy begin
            
            let n =
                min (P.length xs') (P.length ys')
            
            let xs =
                ZL.take n xs' |> C.__force

            let ys =
                ZL.take n ys' |> C.__force

            let zws =
                ZL.zipWithT 0L (fun x y -> C.ret <| x + y) xs ys
                |> C.__force
            
            let zs =
                ZL.zip xs ys
                |> C.__force
                |> ZL.map (fun (x,y) -> x + y)
                |> C.__force
            
            zws = zs
        end


Check.QuickThrowOnFailureAll<ListProperties>()
