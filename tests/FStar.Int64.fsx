#I "../.paket/load/net47"
#r "../bin/Zulib.dll"
#load "FsCheck.fsx"
#load "FSharpx.Collections.fsx"

open FsCheck
open System

module C = Zen.Cost.Realized
module P = Prims
module Z64 = FStar.Int64
module Checked = Operators.Checked

type Generators =
    static member Z64() =
        Arb.from<DoNotSize<int64>>
        |> Arb.convert DoNotSize.Unwrap DoNotSize
Arb.register<Generators>()

type I64Properties =
    static member ``zFits equivalent to within int64 range`` (x: P.int) =
        Z64.fits x = (Int64.MinValue <= x && x <= Int64.MaxValue)

    static member ``zAdd equivalent to fsAdd`` (x: Z64.t) (y: Z64.t) =
        begin
        try
            Checked.(+) x y |> ignore
            true
        with
            | _ -> false
        end ==> lazy (Z64.op_Plus_Hat x y = x + y)

    static member ``zSub equivalent to fsSub`` (x: Z64.t) (y: Z64.t) =
        begin
        try
            Checked.(-) x y |> ignore
            true
        with
            | _ -> false
        end ==> lazy (Z64.op_Subtraction_Hat x y = x - y)

    static member ``zMul equivalent to fsMul`` (x: Z64.t) (y: Z64.t) =
        begin
        try
            Checked.(*) x y |> ignore
            true
        with
            | _ -> false
        end ==> lazy (Z64.op_Star_Hat x y = x * y)

    static member ``zDiv equivalent to fsDiv`` (x: Z64.t) (y: Z64.t) =
        y <> 0L ==> lazy (Z64.op_Slash_Hat x y = x / y)

    static member ``zchecked_add equivalent to fschecked_add``
        (x: Z64.t)
        (y: Z64.t) =
        match Z64.checked_add x y with
        | Some z ->
            (z = Checked.(+) x y)
            |> Prop.ofTestable
        | None ->
            (lazy (Checked.(+) x y))
            |> Prop.throws<OverflowException, int64>

    static member ``zchecked_sub equivalent to fschecked_sub``
        (x: Z64.t)
        (y: Z64.t) =
        match Z64.checked_sub x y with
        | Some z ->
            (z = Checked.(-) x y)
            |> Prop.ofTestable
        | None ->
            (lazy (Checked.(-) x y))
            |> Prop.throws<OverflowException, int64>

    static member ``zchecked_mul equivalent to fschecked_mul``
        (x: Z64.t)
        (y: Z64.t) =
        match Z64.checked_mul x y with
        | Some z ->
            (z = Checked.(*) x y)
            |> Prop.ofTestable
        | None ->
            (lazy (Checked.(*) x y))
            |> Prop.throws<OverflowException, int64>

    static member ``zchecked_div x y = None <==> y = 0``
        (x: Z64.t)
        (y: Z64.t) =
        match Z64.checked_div x y with
        | Some z -> z = x / y
        | None -> y = 0L

    static member ``zadd_mod equivalent to fsadd_mod`` (x: Z64.t) (y: Z64.t) =
        Z64.add_mod x y = x + y
    static member ``zsub_mod equivalent to fssub_mod`` (x: Z64.t) (y: Z64.t) =
        Z64.sub_mod x y = x - y
    static member ``zmul_mod equivalent to fsmul_mod`` (x: Z64.t) (y: Z64.t) =
        Z64.mul_mod x y = x * y

Check.QuickThrowOnFailureAll<I64Properties>()
