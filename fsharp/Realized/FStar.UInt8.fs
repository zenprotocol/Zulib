module FStar.UInt8
//open Prims

module Checked = FSharp.Core.Operators.Checked
open FStar.Pervasives.Native

let fits (x: Prims.int): bool = 0L <= x && x <= 0xFFL
let size (x: Prims.int): unit = ()
type uint_t = Prims.int

type uint8 = System.Byte
type byte = uint8
type t = uint8

let v: uint8 -> uint_t = Checked.int64
let uint_to_t: uint_t -> uint8 = Checked.byte

let add (a:uint8) (b:uint8) : uint8 = a + b
let add_mod a b = add a b
let checked_add a b : option<uint8> = try Some (Checked.(+) a b) with | _ -> None

let sub (a:uint8) (b:uint8) : uint8 = a - b
let sub_mod a b = sub a b
let checked_sub a b : option<uint8> = try Some (Checked.(-) a b) with | _ -> None

let mul (a:uint8) (b:uint8) : uint8 = a * b
let mul_mod a b = mul a b
let checked_mul a b : option<uint8> = try Some (Checked.(*) a b) with | _ -> None

let div (a:uint8) (b:uint8) : uint8 = a / b
let checked_div a b : option<uint8> = try Some (a / b) with | _ -> None

let rem (a:uint8) (b:uint8) : uint8 = a % b

(* Comparison operators *)

let eq (a:uint8) (b:uint8) : bool = a = b
let gt (a:uint8) (b:uint8) : bool =  a > b
let gte (a:uint8) (b:uint8) : bool = a >= b
let lt (a:uint8) (b:uint8) : bool = a < b
let lte (a:uint8) (b:uint8) : bool =  a <= b

(* Constant time comparison operators, TODO: check and implement efficiently *)

(* Infix notations *)

let op_Plus_Hat = add
let op_Plus_Percent_Hat = add_mod
let op_Plus_Question_Hat = checked_add
let op_Subtraction_Hat = sub
let op_Subtraction_Percent_Hat = sub_mod
let op_Subtraction_Question_Hat = checked_sub
let op_Star_Hat = mul
let op_Star_Percent_Hat = mul_mod
let op_Star_Question_Hat = checked_mul
let op_Slash_Hat = div
let op_Slash_Question_Hat = checked_div
let op_Percent_Hat = rem
let op_Equals_Hat = eq
let op_Greater_Hat = gt
let op_Greater_Equals_Hat = gte
let op_Less_Hat = lt
let op_Less_Equals_Hat = lte

// Converts a string to a byte array
let private fsharpStringConvert (s: Core.string): Core.byte Core.array =
    s.ToCharArray() |> Collections.Array.map Checked.byte

// Converts a byte array to a string
let private fstarStringConvert: Core.byte Core.array -> Core.string =
    Collections.Array.map Checked.char >> System.String

let show (x : uint8) : Prims.string = fsharpStringConvert (x.ToString())

let showPad (x : uint8) : Prims.string = fsharpStringConvert (x.ToString("D3"))

let read (s : Prims.string) : uint8 = Checked.uint8 (fstarStringConvert s)

//let to_string_hex s = Printf.sprintf "%02x" s
//let to_int s = s
