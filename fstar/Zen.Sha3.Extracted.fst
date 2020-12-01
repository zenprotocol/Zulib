module Zen.Sha3.Extracted

open Zen.Base
open Zen.Cost
open Zen.Types

module Sha3 = Zen.Sha3.Realized
module PK = Zen.PublicKey
module A = Zen.Array
module U32 = FStar.UInt32

val updateCPK: PK.cpk -> Sha3.t -> Sha3.t `cost` 205
let updateCPK (parity, h) s =
  ret s
  >>= Sha3.updateByte parity
  >>= Sha3.updateHash h
  |> inc 7

val updateContractId : contractId -> Sha3.t -> cost Sha3.t 223
let updateContractId (v, h) sha3 =
  Zen.Cost.inc 7
    begin
    ret sha3
    >>= Sha3.updateU32 v
    >>= Sha3.updateHash h
    end

val lockId : lock -> U32.t `cost` 17
let lockId lock =
  begin match lock with
  | FeeLock                  -> U32.uint_to_t 1
  | PKLock _                 -> U32.uint_to_t 2
  | ActivationSacrificeLock  -> U32.uint_to_t 3
  | ContractLock _           -> U32.uint_to_t 4
  | ExtensionSacrificeLock _ -> U32.uint_to_t 5
  | CoinbaseLock _           -> U32.uint_to_t 6
  | DestroyLock              -> U32.uint_to_t 7
  | HighVLock (lid , _)      -> lid
  end
  |> incRet 17

// WARNING: doesn't update HighV locks!
val updateLock :
  (lock:lock)
  -> Sha3.t
  -> Sha3.t `cost` 293
let updateLock lock sha3 =
  let! lid = lockId lock in // 17
  match lock with
  | PKLock hash ->
      inc 60
      begin
      ret sha3
      >>= Sha3.updateU32 lid // 24
      >>= Sha3.updateHash hash // 192
      end
  | ContractLock contractId ->
      inc 29
      begin
      ret sha3
      >>= Sha3.updateU32 lid // 24
      >>= updateContractId contractId // 223
      end
  | FeeLock ->
      inc 252
      begin
      ret sha3
      >>= Sha3.updateU32 lid // 24
      end
  | DestroyLock ->
      inc 252
      begin
      ret sha3
      >>= Sha3.updateU32 lid // 24
      end
  | ActivationSacrificeLock ->
      inc 252
      begin
      ret sha3
      >>= Sha3.updateU32 lid // 24
      end
  | ExtensionSacrificeLock contractId ->
      inc 29
      begin
      ret sha3
      >>= Sha3.updateU32 lid // 24
      >>= updateContractId contractId // 223
      end
  | CoinbaseLock (blockNumber , hash) ->
      inc 36
      begin
      ret sha3
      >>= Sha3.updateU32 lid // 24
      >>= Sha3.updateU32 blockNumber  // 24
      >>= Sha3.updateHash hash // 192
      end
  | HighVLock _ ->
      // You can't update an high version lock!
      inc 276
      begin
      ret sha3
      end


val updateListWith (#a : Type) (#n : nat) :
    (a -> Sha3.t -> Sha3.t `cost` n)
    -> ls: list a
    -> Sha3.t
    -> Sha3.t `cost` (length ls * (n + 4) + 8)
let updateListWith #_ #_ upd ls sha3 =
  Zen.List.foldT (flip upd) sha3 ls
  |> inc 4

val updateArrayWith (#a : Type) (#n : nat) :
    (a -> Sha3.t -> Sha3.t `cost` n)
    -> arr: A.t a
    -> Sha3.t
    -> Sha3.t `cost` (A.length arr * (n + 4) + 8)
let updateArrayWith #_ #_ upd arr sha3 =
  Zen.Array.foldT (flip upd) sha3 arr
  |> inc 4

val hashWith (#a : Type) (#n : nat) :
    (a -> Sha3.t -> Sha3.t `cost` n)
    -> a
    -> hash `cost` (n + 26)
let hashWith #_ #_ upd x =
  ret Sha3.empty
  >>= upd x
  >>= Sha3.finalize
  |> inc 6