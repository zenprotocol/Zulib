module Zen.Crypto

open System.Runtime.InteropServices
open FsBech32
open Operators.Checked
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

let private b16ToByte: byte -> option<byte> = function
    | c when '0'B <= c && c <= '9'B -> Some (c - '0'B)
    | c when 'A'B <= c && c <= 'F'B -> Some (c - 'A'B + 10uy)
    | c when 'a'B <= c && c <= 'f'B -> Some (c - 'a'B + 10uy)
    | _ -> None

let private pairToByte (hi: byte) (lo: byte): byte = hi * 16uy + lo

let private b16ToBytes: byte[] -> option<byte[]> =
    let rec aux (acc: option<list<byte>>) (bs: list<byte>): option<list<byte>> =
        match acc, bs with
        | Some acc, hi::lo::tl ->
            let acc' = Option.map2 pairToByte (b16ToByte hi) (b16ToByte lo)
                       |> Option.map (fun b -> b::acc)
            aux acc' tl
        | acc, [] -> acc
        | _ -> None
    List.ofArray
    >> aux (Some [])
    >> Option.map (Array.ofList >> Array.rev)

let private b16Decode (b16: string): option<byte[]> =
    try Base16.decode b16 with | Failure _ -> None

let private tryPublicKey (bs: byte[]): option<publicKey> =
    if Array.length bs <> 33 then None else
    let pk = Array.create 64 0uy
    match Native.secp256k1_ec_pubkey_parse(context, pk, bs, 33ul) with
    | Native.Result.Ok -> Some pk
    | _ -> None

let parsePublicKey (b16: Prims.string): Cost.t< option<publicKey>, unit > =
    lazy ( if Array.length b16 % 2 <> 0 then None else
           if Array.exists (not << isBase16Char) b16 then None else
           Array.map char b16
           |> System.String
           |> b16Decode
           |> (fun mx -> Option.bind mx tryPublicKey)
    ) |> Cost.C

let parseHash (s: Prims.string): Cost.t<option<byte []>, unit> =
    lazy ( if s.Length <> 64 then None else
           if Array.exists (not << isBase16Char) s then None else
           b16ToBytes s
    ) |> Cost.C
