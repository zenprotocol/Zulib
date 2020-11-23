#I "../.paket/load/net47"
#r "../bin/Zulib.dll"
#load "FsCheck.fsx"
#load "FSharpx.Collections.fsx"

open FsCheck
open System

module C = Zen.Cost.Realized
module P = Prims
module S = FStar.String
module Checked = Operators.Checked

let zToFS: S.t -> string =
    Array.map Convert.ToChar
    >> String

type StringProperties =
    static member ``zstrlen is string length`` (s: S.t) =
        S.strlen s = int64 (s.Length)
    static member ``zlength is string length`` (s: S.t) =
        S.length s = int64 (s.Length)

    static member ``string_at compliant with z3str`` (s: S.t) (i: Prims.int) =
        (0L <= i && i < S.length s) ==>
            lazy (S.at s i = [| s.[Checked.int32 i] |])

    static member ``strcat is string append`` (s1: S.t) (s2: S.t) =
        S.strcat s1 s2 = Array.append s1 s2

    static member ``cat is string append`` (s1: S.t) (s2: S.t) =
        C.__force (S.cat s1 s2) = Array.append s1 s2

Check.QuickThrowOnFailureAll<StringProperties>()
