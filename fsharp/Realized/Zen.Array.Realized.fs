#light "off"
module Zen.Array.Realized

open Microsoft.FSharp.Core.Operators.Checked // For checked casts

module Arr = Microsoft.FSharp.Collections.Array
module Cost = Zen.Cost.Realized

type array<'A> = FSharp.Core.array<'A>
type t<'A> = array<'A>
type indexed<'A, 'Dummy> = array<'A>

let length (arr: array<'A>): Prims.nat =
    Arr.length arr
    |> int64

let init ( _: Prims.nat)
         ( l: Prims.nat)
         ( f: Prims.nat -> Cost.t<'Aa, Prims.unit>)
         : Cost.t< indexed<'Aa, Prims.unit>, Prims.unit> =
    let init_index : int -> 'Aa =
      int64 >> f >> Cost.__force in
    lazy (Arr.init (int l) init_index)
    |> Cost.C

let empty ( _: Prims.unit)
          : indexed<'Aa, Prims.unit> = [||]

let item ( _: Prims.nat)
         ( i : Prims.nat)
         ( arr : indexed<'Aa, Prims.unit> )
         : Cost.t<'Aa, Prims.unit> =
    lazy ( arr.[int i] )
    |> Cost.C

let init_unique (_:indexed<'a, unit>) : unit = ()
let init_item (_:Prims.nat) (_:Prims.nat)
              (_:(Prims.nat -> Cost.t<'a, unit>))
              (_:Prims.nat)
              : unit = ()

let tryMap
    (_:Prims.nat)
    (_:Prims.nat)
    (f: 'A -> Cost.t<FStar.Pervasives.Native.option<'B>, Prims.unit>)
    (arr: indexed<'A, Prims.unit>)
    : Cost.t<FStar.Pervasives.Native.option<indexed<'B, Prims.unit>>, Prims.unit> =
    lazy ( List.ofArray arr
           |> List.map (f >> Cost.__force)
           |> List.fold (fun state mx -> match (state, mx) with
                                         | Some state, Some x -> Some (x::state)
                                         | _ -> None) (Some [])
           |> Option.map (List.rev >> Array.ofList)
    ) |> Cost.C
