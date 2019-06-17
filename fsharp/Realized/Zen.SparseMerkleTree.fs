module Zen.SparseMerkleTree

open Org.BouncyCastle.Crypto.Digests

[<Literal>]
let N = 256UL

let private initialBase = Array.zeroCreate 32

let private setBit (value:byte array) (bit:System.UInt64) =
    let copy = Array.copy value
    let index = int32 bit
    copy.[index/8] <- copy.[index/8] ||| (1uy <<< (7 - index % 8))

    copy

let private isBitSet (value:byte[]) (bit:System.UInt64) =
    let index = int32 bit
    (value.[index / 8] &&& (1uy <<< (7 - index % 8))) <> 0uy

let private computeMultiple (bytes: byte array seq) =
    let hash = Array.zeroCreate 32
    let sha3 = new Sha3Digest(256)

    Seq.iter (fun bytes -> sha3.BlockUpdate(bytes,0,Array.length bytes)) bytes
    sha3.DoFinal(hash, 0) |> ignore
    hash

let private interiorHash left right (height:System.UInt64) _base =
    if left = right then
        seq {yield left;yield right;}
        |> computeMultiple
    else
        let heightBytes =
            if System.BitConverter.IsLittleEndian then
                System.BitConverter.GetBytes height |> Array.rev
            else
                System.BitConverter.GetBytes height

        seq {yield left;yield right;yield heightBytes; yield _base}
        |> computeMultiple

let private baseHash cwt defaultHash _base value =
    match value with
    | Some value ->
        seq {yield cwt; yield value; yield _base;}
        |> computeMultiple
    | None -> defaultHash

let rec private verifyAuditPath cwt defaultHash key hash auditPath height _base =
    if height = 0UL then
        baseHash cwt defaultHash _base hash
    else
        let splitIndex = setBit _base (N - height)

        let left,right=
            match isBitSet key (N - height) with
            | false ->
                let right = List.head auditPath
                let tail = List.tail auditPath
                let left = verifyAuditPath cwt defaultHash key hash tail (height - 1UL) _base

                left,right
            | true ->
                let left = List.head auditPath
                let tail = List.tail auditPath
                let right = verifyAuditPath cwt defaultHash key hash tail (height - 1UL) splitIndex

                left,right

        interiorHash left right height _base

let private verifyHash cwt defaultHash root auditPath key hash =
    let root' = verifyAuditPath cwt defaultHash key hash auditPath N initialBase

    root' = root

let private fstToFsOption value =
    match value with
    | FStar.Pervasives.Native.Some value -> FSharp.Core.Some value
    | FStar.Pervasives.Native.None -> FSharp.Core.None

let verify (cwt: byte[])
           (defaultHash: byte[])
           (root: byte[])
           (auditPath: Prims.list<byte[]>)
           (key: byte[])
           (value: Option<byte[]>)
           : Zen.Cost.Realized.t<Prims.bool, Prims.unit> =
            let value = fstToFsOption value
            lazy (verifyHash cwt defaultHash root auditPath key value) |> Zen.Cost.Realized.C

let private ensureBigEndian =
    if System.BitConverter.IsLittleEndian then
        Array.rev
    else
        id

let serializeU64 value =
        System.BitConverter.GetBytes (value:uint64)
        |> ensureBigEndian
