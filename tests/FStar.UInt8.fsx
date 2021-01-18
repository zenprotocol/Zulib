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
module Z8 = FStar.UInt8
module Checked = Operators.Checked

type Generators =
    static member I64() =
        Arb.from<DoNotSize<int64>>
        //|> Arb.convert DoNotSize.Unwrap DoNotSize
Arb.register<Generators>()

type U8Properties =
    static member ``zFits equivalent to within uint8 range`` (x: P.int) =
        match Z8.fits x with
        | true ->
            (
            Byte.MinValue <= Checked.uint8 x
            && Checked.uint8 x <= Byte.MaxValue)
            |> Prop.ofTestable
        | false ->
            lazy (Checked.uint8 x)
            |> Prop.throws<OverflowException, uint8>

    static member ``zAdd equivalent to fsAdd`` (x: Z8.t) (y: Z8.t) =
        begin
        try
            Checked.(+) x y |> ignore
            true
        with
            | _ -> false
        end ==> lazy (Z8.op_Plus_Hat x y = x + y)

    static member ``zSub equivalent to fsSub`` (x: Z8.t) (y: Z8.t) =
        begin
        try
            Checked.(-) x y |> ignore
            true
        with
            | _ -> false
        end ==> lazy (Z8.op_Subtraction_Hat x y = x - y)

    static member ``zMul equivalent to fsMul`` (x: Z8.t) (y: Z8.t) =
        let x = x / 10uy
        let y = y / 10uy
        begin
        try
            Checked.(*) x y |> ignore
            true
        with
            | _ -> false
        end ==> lazy (Z8.op_Star_Hat x y = x * y)

    static member ``zDiv equivalent to fsDiv`` (x: Z8.t) (y: Z8.t) =
        y <> 0uy ==> lazy (Z8.op_Slash_Hat x y = x / y)

    static member ``zchecked_add equivalent to fschecked_add``
        (x: Z8.t)
        (y: Z8.t) =
        match Z8.checked_add x y with
        | Some z ->
            (z = Checked.(+) x y)
            |> Prop.ofTestable
        | None ->
            (lazy (Checked.(+) x y))
            |> Prop.throws<OverflowException, uint8>

    static member ``zchecked_sub equivalent to fschecked_sub``
        (x: Z8.t)
        (y: Z8.t) =
        match Z8.checked_sub x y with
        | Some z ->
            (z = Checked.(-) x y)
            |> Prop.ofTestable
        | None ->
            (lazy (Checked.(-) x y))
            |> Prop.throws<OverflowException, uint8>

    static member ``zchecked_mul equivalent to fschecked_mul``
        (x: Z8.t)
        (y: Z8.t) =
        match Z8.checked_mul x y with
        | Some z ->
            (z = Checked.(*) x y)
            |> Prop.ofTestable
        | None ->
            (lazy (Checked.(*) x y))
            |> Prop.throws<OverflowException, uint8>

    static member ``zchecked_div x y = None <==> y = 0``
        (x: Z8.t)
        (y: Z8.t) =
        match Z8.checked_div x y with
        | Some z -> z = x / y
        | None -> y = 0uy

    static member ``zadd_mod equivalent to fsadd_mod`` (x: Z8.t) (y: Z8.t) =
        Z8.add_mod x y = x + y
    static member ``zsub_mod equivalent to fssub_mod`` (x: Z8.t) (y: Z8.t) =
        Z8.sub_mod x y = x - y
    static member ``zmul_mod equivalent to fsmul_mod`` (x: Z8.t) (y: Z8.t) =
        Z8.mul_mod x y = x * y


    static member ``of_string to_string isomorphism`` (x : Z8.t) : bool =
        match Z8.of_string (Z8.to_string x) with
        | Some y -> x = y
        | None -> false

Check.QuickThrowOnFailureAll<U8Properties>()
