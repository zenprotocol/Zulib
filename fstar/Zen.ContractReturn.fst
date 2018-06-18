module Zen.ContractReturn

open Zen.Base
open Zen.Types
open Zen.Cost

type t = contractReturn

val ofTxSkel: txSkeleton -> contractReturn `cost` 3
let ofTxSkel txSkel = {tx=txSkel; message=None; state=NoChange} |> incRet 3

val setTx: txSkeleton -> contractReturn -> contractReturn `cost` 3
let setTx txSkel cReturn = {cReturn with tx=txSkel} |> incRet 3

val setMessage: message -> contractReturn -> contractReturn `cost` 3
let setMessage msg cReturn = {cReturn with message=Some msg} |> incRet 3

val setState: stateUpdate -> contractReturn -> contractReturn `cost` 3
let setState sUpdate cReturn = {cReturn with state=sUpdate} |> incRet 3

let setStateDelete: contractReturn -> contractReturn `cost` 3 =
    setState Delete
let setStateNoChange: contractReturn -> contractReturn `cost` 3 =
    setState NoChange
let setStateUpdate: data -> contractReturn -> contractReturn `cost` 3 =
    setState << Update
