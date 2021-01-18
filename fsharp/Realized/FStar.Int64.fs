module FStar.Int64
//open Prims

module Checked = FSharp.Core.Operators.Checked
open FStar.Pervasives.Native

let fits (x: Prims.int): bool = -0x8000000000000000L <= x && x <= 0x7FFFFFFFFFFFFFFFL
let size (x: Prims.int): unit = ()
type int_t = Prims.int

type int64 = System.Int64
type t = int64

let v: int64 -> int_t = Checked.int64
let int_to_t: int_t -> int64 = Checked.int64

let add (a:int64) (b:int64) : int64 = a + b
let add_mod a b = add a b
let checked_add a b : option<int64> = try Some (Checked.(+) a b) with | _ -> None

let sub (a:int64) (b:int64) : int64 = a - b
let sub_mod a b = sub a b
let checked_sub a b : option<int64> = try Some (Checked.(-) a b) with | _ -> None

let mul (a:int64) (b:int64) : int64 = a * b
let mul_mod a b = mul a b
let checked_mul a b : option<int64> = try Some (Checked.(*) a b) with | _ -> None

let div (a:int64) (b:int64) : int64 = a / b
let checked_div a b : option<int64> = try Some (a / b) with | _ -> None

let rem (a:int64) (b:int64) : int64 = a % b

(* Comparison operators *)

let eq (a:int64) (b:int64) : bool = a = b
let gt (a:int64) (b:int64) : bool =  a > b
let gte (a:int64) (b:int64) : bool = a >= b
let lt (a:int64) (b:int64) : bool = a < b
let lte (a:int64) (b:int64) : bool =  a <= b

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

let of_string (s : Prims.string) : t option =
    try
        Some (Prims.parse_int s)
    with _ ->
        None

let to_string (x : t) : Prims.string =
    Prims.to_string (v x)

//let to_string_hex s = Printf.sprintf "%02x" s
//let to_int s = s
