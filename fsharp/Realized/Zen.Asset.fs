module Zen.Asset

open Zen.Pervasives
open Zen.Types.Extracted
open System
open System.Text
open Zen.UInt32
open FsBech32
open Zen.Pervasives

module Cost = Zen.Cost.Realized
module ContractId = Zen.ContractId

let private filler len =
    32 - len
    |> Array.zeroCreate

[<Literal>]
let NoSubIdentifierBytesLength = ContractId.BytesLength
[<Literal>]
let SubIdentifierBytesLength = 68 // NoSubIdentifierBytesLength + 32
[<Literal>]
let NoSubIdentifierEncodedBytesLength = 72 // 2 * NoSubIdentifierBytesLength
[<Literal>]
let SubIdentifierEncodedBytesLength = 136 // 2 * SubIdentifierBytesLength

let private getBytes value = BitConverter.GetBytes (value:uint32)

let private getBigEndinanBytes =
    getBytes
    >> if BitConverter.IsLittleEndian
        then Array.rev
        else id

let zeroHash = Array.zeroCreate 32

let zenAsset : asset = 0ul, zeroHash, zeroHash

let private decodeB16Bytes: Prims.string -> option< array<byte> > =
    System.Text.Encoding.ASCII.GetString >> Base16.decode

let parse (value : Prims.string) : Cost.t<Native.option<asset>, unit> =
    lazy (
        if value = "00"B then Native.Some zenAsset else
        let l = Array.length value
        if not (l = SubIdentifierEncodedBytesLength || l = NoSubIdentifierEncodedBytesLength)
        then Native.None
        else
            let contractID = ContractId.parse value.[0..ContractId.EncodedBytesLength-1]
                             |> Cost.__force
            let subID = if l = NoSubIdentifierEncodedBytesLength
                        then Some zeroHash
                        else decodeB16Bytes value.[ContractId.EncodedBytesLength..]
            match contractID, subID with
            | Native.Some (ver, cHash), Some subID ->
                Native.Some (ver, cHash, subID)
            | _ ->
                Native.None
    )
    |> Cost.C

let getDefault ((version,cHash) : contractId) : Cost.t<asset, unit> =
    lazy (version, cHash, zeroHash)
    |> Cost.C

let fromSubtypeString ((version,cHash) : contractId) (value : Prims.string) : Cost.t<asset, unit> =
    lazy (
        let n = Array.length value
        let bytes = Array.append value (filler n)
        version, cHash, bytes
    )
    |> Cost.C

let fromSubtypeInt ((version,cHash) : contractId) (value : uint32) : Cost.t<asset, unit> =
    lazy (
        let bytes = Array.append [| 0uy |] (getBigEndinanBytes value)
        let bytes = Array.append bytes (filler (Array.length bytes))
        version, cHash, bytes
    )
    |> Cost.C
