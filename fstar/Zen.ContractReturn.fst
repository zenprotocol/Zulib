module Zen.ContractReturn

open Zen.Base
open Zen.Types

type t = contractReturn

val ofTxSkel: txSkeleton -> contractReturn
let ofTxSkel txSkel = {tx=txSkel; message=None; state=NoChange}

val setTx: txSkeleton -> contractReturn -> contractReturn
let setTx txSkel contractReturn = {contractReturn with tx=txSkel}

val setMessage: message -> contractReturn -> contractReturn
let setMessage msg contractReturn = {contractReturn with message=Some msg}

val setState: stateUpdate -> contractReturn -> contractReturn
let setState sUpdate contractReturn = {contractReturn with state=sUpdate}

let setStateDelete: contractReturn -> contractReturn =
    setState Delete
let setStateNoChange: contractReturn -> contractReturn =
    setState NoChange
let setStateUpdate: data -> contractReturn -> contractReturn =
    setState << Update
