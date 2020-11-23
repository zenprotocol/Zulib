#I "../.paket/load/net47"
#r "../bin/Zulib.dll"
#load "FsCheck.fsx"
#load "FSharpx.Collections.fsx"

open FsCheck

module C = Zen.Cost.Realized
module P = Prims
module S = FStar.String
module ZD = Zen.Dictionary

type FSDict<'A> =
    Map<S.t, 'A>

// Custom generators for ZD.t don't register for some reason
// This DU is a hack to fix that
type ZDict<'A> =
    Z of ZD.t<'A> with
    static member  v(Z d): ZD.t<'A> = d
    static member  z(d: FSDict<'A>): ZDict<'A> =
        Z (d, uint32 d.Count)
    static member fs(Z d): FSDict<'A> = fst d

type Generators =
    static member ZDict<'a>() =
        Arb.from<FSDict<'a>>
        |> Arb.convert ZDict.z ZDict.fs
Arb.register<Generators>()

let force: C.t<'A, 'B> -> 'A = C.__force

type DictProperties =

    static member ``roundtrip fs -> z -> fs`` (d: FSDict<int>) =
        d |> ZDict.z |> ZDict.fs = d
    static member ``roundtrip z -> fs -> z`` (d: ZDict<int>) =
        d |> ZDict.fs |> ZDict.z = d

    static member ``zSize equivalent to fsCount`` (d: ZDict<int>) =
        ZD.size (ZDict.v d) = int64 ((ZDict.fs d).Count)

    static member ``zContainsKey equivalent to fsContainsKey``
        (k: S.t)
        (d: ZDict<int>) =
        let zContainsKey = ZD.containsKey k (ZDict.v d)
        let fsContainsKey = (ZDict.fs d).ContainsKey(k)
        force zContainsKey = fsContainsKey

    static member ``zAdd equivalent to fsAdd``
        (x: int)
        (k: S.t)
        (d: ZDict<int>) =
        let zAdd = ZD.add k x (ZDict.v d)
        let fsAdd = (ZDict.fs d).Add(k, x)
        Z (force zAdd) = ZDict.z fsAdd

    static member ``zRemove equivalent to fsRemove`` (k: S.t) (d: ZDict<int>) =
        let zRemove = ZD.remove k (ZDict.v d)
        let fsRemove = (ZDict.fs d).Remove(k)
        Z (force zRemove) = ZDict.z fsRemove

    static member ``zTryFind equivalent to fsTryFind``
        (k: S.t)
        (d: ZDict<int>) =
        let zTryFind = ZD.tryFind k (ZDict.v d)
        let fsTryFind = (ZDict.fs d).TryFind(k)
        force zTryFind = fsTryFind

Check.QuickThrowOnFailureAll<DictProperties>()
