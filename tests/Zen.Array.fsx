#I "../.paket/load/net47"
#r "../bin/Zulib.dll"
#load "FsCheck.fsx"
#load "FSharpx.Collections.fsx"

open FsCheck

module C = Zen.Cost.Realized
module P = Prims
module ZAB = Zen.Array.Base
module ZAI = Zen.Array.Indexed

type ArrayProperties =

    static member ``zLength equivalent to fsLength`` (zs: int[]) =
        ZAB.length zs = int64 (zs.Length)

    static member ``zItem equivalent to fsItem`` (i: int) (zs: int[]) =
        (0 < i && i < zs.Length) ==>
        lazy ( let zItem = ZAI.item (ZAB.length zs) (int64 i) zs
               let fsItem = zs.[i]
               zItem = fsItem )

    static member ``zInit equivalent to fsInit`` (zs: int[]) =
        let n = ZAB.length zs
        (n <> 0L && n < int64 System.Int32.MaxValue) ==>
        lazy ( let zInit = ZAI.init 0L n (fun i -> ZAI.item (ZAB.length zs) i zs |> C.ret)
               let fsInit = Array.init (int n) (fun i -> zs.[i])
               C.__force zInit = fsInit )

    static member ``zGet equivalent to fsItem`` (i: int) (zs: int[]) =
        (0 < i && i < zs.Length) ==>
        lazy ( let zGet = ZAB.get (ZAB.length zs) zs (int64 i)
               let fsItem = zs.[i]
               zGet = fsItem )

    static member ``zAt equivalent to fsItem`` (i: int) (zs: int[]) =
        (0 < i && i < zs.Length) ==>
        lazy ( let zAt = ZAB.get (ZAB.length zs) zs (int64 i)
               let fsItem = zs.[i]
               zAt = fsItem )

    static member ``zCreate equivalent to fsCreate`` (n: int) (x: int) =
        0 < n ==> lazy ( let zCreate = ZAI.create (int64 n) x
                         let fsCreate = Array.create n x
                         C.__force zCreate = fsCreate )

    static member ``zSub equivalent to fsSub``
        (i: NonNegativeInt)
        (j: NonNegativeInt)
        (zs: int[]) =
        let i = int i
        let j = int j
        let n = zs.Length
        i + j <= n ==>
        lazy ( let zSub = ZAI.sub (int64 n) zs (int64 i) (int64 j)
               let fsSub = Array.sub zs i j
               C.__force zSub = fsSub )

    static member ``zMap equivalent to fsMap``
        (zs: int[])
        (f: Function<int, byte>) =
        let f = f.Value
        let zMap = ZAI.map 0L (ZAB.length zs) (f >> C.ret) zs
        let fsMap = Array.map f zs
        C.__force zMap = fsMap

    static member ``zAppend equivalent to fsAppend`` (xs: int[]) (ys: int[]) =
        let zAppend = ZAI.append (ZAB.length xs) (ZAB.length ys) xs ys
        let fsAppend = Array.append xs ys
        C.__force zAppend = fsAppend

    static member ``zChunkBySize equivalent to fsChunkBySize\last``
        (sz: PositiveInt)
        (zs: int[]) =
        let sz = int sz
        let zChunks = ZAI.chunkBySize (ZAB.length zs) (int64 sz) zs
        let fsChunks = Array.chunkBySize sz zs
        (C.__force zChunks).Length = zs.Length / sz
        &&
        if zs.Length % sz <> 0
        then C.__force zChunks = Array.sub fsChunks 0 (fsChunks.Length - 1)
        else C.__force zChunks = fsChunks

Check.QuickAll<ArrayProperties>()
