module Zen.ContractId

open Zen.Types.Extracted
open System

module Seq = FSharpx.Collections.Seq
module Cost = Zen.Cost.Realized

[<Literal>]
let BytesLength = 36 // 4 + 32
[<Literal>]
let EncodedBytesLength = 72 // 2 * BytesLength

let private charToByte: FStar.Char.t -> byte = function
    | c when 'A'B <= c && c <= 'F'B -> c - 'A'B + 0x0Auy
    | c when 'a'B <= c && c <= 'f'B -> c - 'a'B + 0x0auy
    | c when '0'B <= c && c <= '9'B -> c - '0'B
    | _ -> failwith "Not a valid hex character"

let private isHexChar: FStar.Char.t -> bool = function
    | c when ('A'B <= c && c <= 'F'B) ||
             ('a'B <= c && c <= 'f'B) ||
             ('0'B <= c && c <= '9'B)
        -> true
    | _ -> false

let private parseInt: array<byte> -> uint32 =
    let toUInt32 (bytes: array<byte>) =
        BitConverter.ToUInt32(bytes, 0)
    if BitConverter.IsLittleEndian
    then Array.rev >> toUInt32
    else toUInt32

let parse (value : Prims.string) : Cost.t<contractId FStar.Pervasives.Native.option, unit> =
    lazy (
        if value.Length <> EncodedBytesLength then None else
        if not (Array.forall isHexChar value) then None else
        value |> Array.map charToByte
              |> Array.chunkBySize 2
              |> Array.map (fun [|x; y|] -> x * 0x10uy + y)
              |> Array.splitAt 4
              |> begin fun (first4, last32) ->
                    parseInt first4, last32
                 end
              |> Some
    ) |> Cost.C
