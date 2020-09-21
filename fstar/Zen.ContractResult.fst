module Zen.ContractResult

open Zen.Base
open Zen.Types
open Zen.Cost
module RT = Zen.ResultT
module CReturn = Zen.ContractReturn

type t = contractResult

let ofTxSkel: txSkeleton -> contractResult `cost` 3 =
    CReturn.ofTxSkel >> RT.liftCost

let ofResultTxSkel(tx: result txSkeleton): contractResult `cost` 3 =
    let open RT in
    RT.liftRes tx >>= ofTxSkel

val ofOptionTxSkel: string -> option txSkeleton -> contractResult `cost` 3
let ofOptionTxSkel msg tx =
    RT.ofOption msg tx
    >>= ofResultTxSkel

let setMessage (msg: message) (cRes: contractResult): contractResult `cost` 3 =
    let open RT in
    RT.liftRes cRes >>= (CReturn.setMessage msg >> RT.liftCost)
let setState (update: stateUpdate) (cRes: contractResult): contractResult `cost` 3 =
    let open RT in
    RT.liftRes cRes >>= (CReturn.setState update >> RT.liftCost)
let setStateDelete: contractResult -> contractResult `cost` 3 =
    setState Delete
let setStateNoChange: contractResult -> contractResult `cost` 3 =
    setState NoChange
let setStateUpdate: data -> contractResult -> contractResult `cost` 3 =
    setState << Update
