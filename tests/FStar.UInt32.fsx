#I "../.paket/load/net47"
#r "../packages/BouncyCastle/lib/BouncyCastle.Crypto.dll"
#r "../packages/FSharp.Compatibility.OCaml/lib/net45/FSharp.Compatibility.OCaml.dll"
#r "../bin/Zulib.dll"
#load "FsCheck.fsx"
#load "FSharpx.Collections.fsx"

open FsCheck
open System

module C = Zen.Cost.Realized
module P = Prims
module Z32 = FStar.UInt32
module Checked = Operators.Checked

type Generators =
    static member I64() =
        Arb.from<DoNotSize<int64>>
        //|> Arb.convert DoNotSize.Unwrap DoNotSize
    static member Z32() =
        Arb.from<DoNotSize<uint32>>
        //|> Arb.convert DoNotSize.Unwrap DoNotSize
Arb.register<Generators>()

type U32Properties =
    static member ``zFits equivalent to within uint32 range`` (x: P.int) =
        match Z32.fits x with
        | true ->
            (UInt32.MinValue <= Checked.uint32 x
            && Checked.uint32 x <= UInt32.MaxValue)
            |> Prop.ofTestable
        | false ->
            lazy (Checked.uint32 x)
            |> Prop.throws<OverflowException, uint32>

    static member ``zAdd equivalent to fsAdd`` (x: Z32.t) (y: Z32.t) =
        begin
        try
            Checked.(+) x y |> ignore
            true
        with
            | _ -> false
        end ==> lazy (Z32.op_Plus_Hat x y = x + y)

    static member ``zSub equivalent to fsSub`` (x: Z32.t) (y: Z32.t) =
        begin
        try
            Checked.(-) x y |> ignore
            true
        with
            | _ -> false
        end ==> lazy (Z32.op_Subtraction_Hat x y = x - y)

    static member ``zMul equivalent to fsMul`` (x: Z32.t) (y: Z32.t) =
        begin
        try
            Checked.(*) x y |> ignore
            true
        with
            | _ -> false
        end ==> lazy (Z32.op_Star_Hat x y = x * y)

    static member ``zDiv equivalent to fsDiv`` (x: Z32.t) (y: Z32.t) =
        y <> 0u ==> lazy (Z32.op_Slash_Hat x y = x / y)

    static member ``zchecked_add equivalent to fschecked_add``
        (x: Z32.t)
        (y: Z32.t) =
        match Z32.checked_add x y with
        | Some z ->
            (z = Checked.(+) x y)
            |> Prop.ofTestable
        | None ->
            (lazy (Checked.(+) x y))
            |> Prop.throws<OverflowException, uint32>

    static member ``zchecked_sub equivalent to fschecked_sub``
        (x: Z32.t)
        (y: Z32.t) =
        match Z32.checked_sub x y with
        | Some z ->
            (z = Checked.(-) x y)
            |> Prop.ofTestable
        | None ->
            (lazy (Checked.(-) x y))
            |> Prop.throws<OverflowException, uint32>

    static member ``zchecked_mul equivalent to fschecked_mul``
        (x: Z32.t)
        (y: Z32.t) =
        match Z32.checked_mul x y with
        | Some z ->
            (z = Checked.(*) x y)
            |> Prop.ofTestable
        | None ->
            (lazy (Checked.(*) x y))
            |> Prop.throws<OverflowException, uint32>

    static member ``zchecked_div x y = None <==> y = 0``
        (x: Z32.t)
        (y: Z32.t) =
        match Z32.checked_div x y with
        | Some z -> z = x / y
        | None -> y = 0u

    static member ``zadd_mod equivalent to fsadd_mod`` (x: Z32.t) (y: Z32.t) =
        Z32.add_mod x y = x + y
    static member ``zsub_mod equivalent to fssub_mod`` (x: Z32.t) (y: Z32.t) =
        Z32.sub_mod x y = x - y
    static member ``zmul_mod equivalent to fsmul_mod`` (x: Z32.t) (y: Z32.t) =
        Z32.mul_mod x y = x * y
    
    static member ``of_string to_string isomorphism`` (x : Z32.t) : bool =
        match Z32.of_string (Z32.to_string x) with
        | Some y -> x = y
        | None -> false

Check.QuickThrowOnFailureAll<U32Properties>()
