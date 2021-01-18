module Test.FStar.String

open Zen.Cost
open Zen.Base
open FStar.String

module A = Zen.Array

let test_strlen : nat =
    strlen "ABCD" + 3

let test_length : nat =
    length "ABCD" + 3

let test_toChars : nat =
    A.length (toChars "ABCD")

let test_ofChars : nat `cost` (4 * 2 + 2) =
    A.ofList [ 65uy ; 66uy ; 67uy ; 68uy ]
    $> ofChars
    $> length

let test_at : nat =
    length (at "ABCD" 2)

let test_strcat : nat =
    length (strcat "AB" "CD")
