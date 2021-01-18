module Zen.PublicKey

open Zen.Types
open Zen.Cost

module Arr = Zen.Array



// Compressed public key
type cpk = byte ** hash



let compress (pk: publicKey) : cost cpk 305 =
    begin
    inc 13
        begin
        let open FStar.UInt8 in
        let aux (i: nat{i < 32}) : cost byte 5 = (inc 5 (ret (Arr.item (31 - i) pk))) in
        let parity = (Arr.item 32 pk %^ 2uy) +^ 2uy in
        let! x = Arr.init_pure 32 aux in
        ret (parity, x)
        end
    end
