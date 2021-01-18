module Test.Zen.Array.Base

open Zen.Base
open Zen.Cost

module A = Zen.Array.Base

let example_ofList_indexed : A.indexed nat 3 `cost` (3 * 2 + 2) =
    A.ofList [ 1 ; 2 ; 3 ]

let example_ofList : A.t nat `cost` (3 * 2 + 2) =
    A.ofList [ 1 ; 2 ; 3 ]
    $> (fun arr -> arr <: A.t nat)

let test_length : nat `cost` (3 * 2 + 2) =
    example_ofList_indexed
    $> A.length
    $> (fun (x : nat) -> (x + 5) <: nat)

let test_get : nat `cost` (3 * 2 + 2) =
    example_ofList_indexed
    $> (fun arr -> A.get arr 2)
    $> (fun (x : nat) -> (x + 5) <: nat)

let test_empty : nat =
    A.length (A.empty <: A.indexed nat 0) + 5

let test_fold : nat `cost` (4 * 0 + 4) =
    A.fold (fun (x y : nat) -> (x + y) <: nat) (0 <: nat) A.empty
    $> (fun (x : nat) -> (x + 5) <: nat)

let test_foldT : nat `cost` (0 * (0 + 4) + 4) =
    A.foldT (fun (x y : nat) -> ret ((x + y) <: nat)) (0 <: nat) A.empty
    $> (fun (x : nat) -> (x + 5) <: nat)
