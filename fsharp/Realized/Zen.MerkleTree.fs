module Zen.MerkleTree

let private findSplitIndex (length: Prims.nat): Prims.nat =
    Seq.initInfinite (fun i -> pown 2L i)
    |> Seq.find (fun i -> i * 2L >= length)

let verify (root: Zen.Types.Extracted.hash)
           (auditPath: Prims.list<Zen.Types.Extracted.hash>)
           (index: Prims.nat)
           (hash: Zen.Types.Extracted.hash)
           : Zen.Cost.Realized.t<Prims.bool, Prims.unit> =
    lazy (
        let rec verify' auditPath index =
            lazy (
                match auditPath with
                | Prims.Nil -> hash
                | Prims.Cons(head, tail) ->
                    let length = max (pown 2L (int (Prims.length auditPath)))
                                     (index + 1L)
                    let splitIndex = findSplitIndex length

                    let leftHash,rightHash =
                        match index < splitIndex with
                        | true ->
                            let leftHash = verify' tail index
                                           |> Zen.Cost.Realized.__force
                            leftHash, head
                        | false ->
                            let rightHash = verify' tail (index - splitIndex)
                                            |> Zen.Cost.Realized.__force
                            head,rightHash

                    Zen.Hash.Sha3.empty
                    |> Zen.Hash.Sha3.updateHash leftHash
                    |> Zen.Cost.Realized.__force
                    |> Zen.Hash.Sha3.updateHash rightHash
                    |> Zen.Cost.Realized.__force
                    |> Zen.Hash.Sha3.finalize
                    |> Zen.Cost.Realized.__force
            ) |> Zen.Cost.Realized.C
        let root' = verify' auditPath index
                    |> Zen.Cost.Realized.__force
        root' = root
    ) |> Zen.Cost.Realized.C
