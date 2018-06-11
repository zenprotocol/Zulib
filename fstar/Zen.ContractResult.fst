module Zen.ContractResult

open Zen.Base
open Zen.Types
open Zen.Cost

module R = Zen.Result
module CReturn = Zen.ContractReturn
type t = contractResult

let ofTxSkel: txSkeleton -> contractResult =
    CReturn.ofTxSkel >> OK

let ofResultTxSkel: result txSkeleton -> contractResult =
    R.map CReturn.ofTxSkel

val ofOptionTxSkel: string -> option txSkeleton -> contractResult
let ofOptionTxSkel msg =
    R.ofOption "msg"
    >> ofResultTxSkel

let setMessage: message -> contractResult -> contractResult =
    R.map << CReturn.setMessage
let setState: stateUpdate -> contractResult -> contractResult =
    R.map << CReturn.setState
let setStateDelete: contractResult -> contractResult =
    setState Delete
let setStateNoChange: contractResult -> contractResult =
    setState NoChange
let setStateUpdate: data -> contractResult -> contractResult =
    setState << Update
