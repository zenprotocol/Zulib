module Zen.Crypto

open System.Runtime.InteropServices
open FsBech32
open Zen.Types.Extracted

module Cost = Zen.Cost.Realized

module Native =
    let SECP256K1_FLAGS_TYPE_CONTEXT = (1ul <<< 0)
    let SECP256K1_FLAGS_BIT_CONTEXT_VERIFY = (1ul <<< 8)
    let SECP256K1_CONTEXT_VERIFY = (SECP256K1_FLAGS_TYPE_CONTEXT ||| SECP256K1_FLAGS_BIT_CONTEXT_VERIFY)

    type Context = System.IntPtr

    type Result =
        | Error = 0
        | Ok = 1

    // Create a secp256k1 context object.
    [<DllImport("secp256k1", CallingConvention=CallingConvention.Cdecl)>]
    extern Context secp256k1_context_create(uint32 flags);

    [<DllImport("secp256k1", CallingConvention=CallingConvention.Cdecl)>]
    extern Result secp256k1_ecdsa_verify(
        Context ctx,
        byte[] signature,
        byte[] msg32,
        byte[] pubkey
    );

    [<DllImport("secp256k1", CallingConvention=CallingConvention.Cdecl)>]
    extern Result secp256k1_ec_pubkey_parse(
        Context ctx,
        byte[] pubkey,
        byte[] input,
        uint32 inputlen
    );

let private context = Native.secp256k1_context_create (Native.SECP256K1_CONTEXT_VERIFY)

let verify (pk:publicKey) (signature:signature) (msg:hash) : Cost.t<bool, unit> =
    lazy (
        match Native.secp256k1_ecdsa_verify (context, signature, msg, pk) with
        | Native.Result.Ok -> true
        | _ -> false
    )
    |> Cost.C

let private isBase16Char (c: byte): bool =
    ('0'B <= c && c <= '9'B)
    || ('A'B <= c && c <= 'F'B)
    || ('a'B <= c && c <= 'f'B)

let private b16ToByte: byte -> byte = function
    | c when '0'B <= c && c <= '9'B -> c - '0'B
    | c when 'A'B <= c && c <= 'F'B -> c - 'A'B + 10uy
    | c when 'a'B <= c && c <= 'f'B -> c - 'a'B + 10uy
    | _ -> failwith "Not a valid base 16 char"

let private b16ToBytes: byte array -> byte array =
    Array.map b16ToByte
    >> Array.chunkBySize 2
    >> Array.map (fun [|hi; lo|] -> hi * 16uy + lo)

let parsePublicKey (bs16:byte array) : Cost.t<FStar.Pervasives.Native.option<publicKey>, unit> =
    lazy (
        let bs16 = System.Text.Encoding.ASCII.GetString bs16
        match Base16.decode bs16 with
        | None -> FStar.Pervasives.Native.None
        | Some bytes ->
            if Array.length bytes <> 33 then
                FStar.Pervasives.Native.None
            else
                let publicKey = Array.create 64 0uy

                match Native.secp256k1_ec_pubkey_parse(context, publicKey, bytes, 33ul) with
                | Native.Result.Ok -> FStar.Pervasives.Native.Some publicKey
                | _ -> FStar.Pervasives.Native.None
    ) |> Cost.C

let parseHash (s: Prims.string): Cost.t<FStar.Pervasives.Native.option<byte []>, unit> =
    lazy (
        if s.Length <> 64 then FStar.Pervasives.Native.None else
        if not <| Array.forall isBase16Char s then FStar.Pervasives.Native.None else
        s |> b16ToBytes
          |> FStar.Pervasives.Native.Some
    ) |> Cost.C
