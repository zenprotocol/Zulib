#light "off"
module Zen.Array.Base

open Microsoft.FSharp.Core.Operators.Checked // For checked casts

module Arr = Microsoft.FSharp.Collections.Array
module Cost = Zen.Cost.Realized

type t<'A> = array<'A>
type indexed<'A, 'Dummy> = array<'A>

let length (arr: array<'A>): Prims.nat =
    Arr.length arr
    |> int64

let get ( _: Prims.nat)
        ( arr : indexed<'Aa, Prims.unit> )
        ( i : Prims.nat)
        : 'Aa =
    arr.[int i]

let empty (): indexed<'Aa, Prims.unit> = [||]

let empty_uniq (): unit = ()

let ofList (ls: Prims.list<'A>): Cost.t<indexed<'A, Prims.unit>, Prims.unit> =
    let rec toList acc = function
        | Prims.Nil -> acc
        | Prims.Cons(hd, tl) -> toList (hd::acc) tl
    in
    lazy(
        ls |> toList[]
           |> List.rev
           |> Array.ofList
    ) |> Cost.C

let ofList_get (_: Prims.list<'A>) (_:Prims.nat): unit = () 

let fold  (f : 's -> 'a -> 's) (x : 's) (arr : 'a[]) : Cost.t<'s , Prims.unit> = 
    Cost.ret <| Array.fold f x arr

let foldT (_:Prims.nat) (f : 's -> 'a -> Cost.t<'s , Prims.unit>) (x : 's) (arr : 'a[]) : Cost.t<'s , Prims.unit> =
    Array.fold (f << Cost.__force) (Cost.ret x) arr
