# Zulib

The Zulib library replaces the `ulib` library of F* to incorporate primitives and basic functions used in smart contracts and to force smart contracts to use only functions which carry a cost (unless explicitly omitting it, as in the case of the `length` function for lists).

# Compilation

To compile the project, first clone it from the git repository by:

```bash
git clone git@github.com:zenprotocol/Zulib.git
```

And then prepare and compile it with:

```bash
cd Zulib
./paket restore
./build.sh
```

See the *Commands* section for more info on `build.sh`.

# Library Structure

There are 3 kind of modules:

1. Extracted modules - Both declarations and implementations are written in ZF*.
2. Realized modules - Declerations are written in ZF*, implementations are written in F#.
3. Code-gen modules - Unifying multiple modules under a single prefix.

Extracted modules are translated to F# during the compilation of the library and their translation is created in the `fsharp/Extracted` directory when running the extraction (`./build.sh Extract`) command.

Realized modules are already implemented in F# and their implementation code is in the  `fsharp/Realized` directory, moreover - type and function declerations with the `assume` modifier are explicitly implemented in F#, and all `.fsti` modules are realized modules since they contain only declerations.

All modules are bundled into the `bin/Zulib.dll` file during the compilation (`./build.sh Build`) of the library.

When contracts are compiled with the `zebra` tool, they are linked with the `bin/Zulib.dll` file to provide the contract with access to Zulib modules.

# Commands

The `build.sh` program calls the `build.fsx` script, which supports the following commands:

- `Clean` - Clean all generated (`.dll`, `.fs`, etc.) files (not including consistency files)
- `RecordHints` - Record Z3 hints for faster verification of the Zulib modules.
- `Verify` - Verify (type check) the Zulib modules.
- `Extract` - Extract the extracted Zulib modules into the `fsharp/Extracted` directory.
- `Build` - Compile the library and crate the `bin/Zulib.dll` file.
- `Consistency` - Run consistency tests to verify the consistency of the realized modules with the rest of the library (**note:** currently done by manually written files which import and use functions from those modules, located in the `tests/consistency` directory. can probably be automated)
- `ConsistencyClean` - Clean all the generated consistency F# files.
- `Test` - Run the runtime test in the `tests` directory.

You can call the commands directly from `build.sh` file, for example to run the tests run:

```bash
./build.sh Test
```

The usual build is done by simply running:

```bash
./build.sh
```

which executes the `Clean`, `Verify`, `Extract`, and `Build` commands, in that order.

See the code in the `build.fsx` for the implementation details.

**Known Issues:** Sometimes when building you'll get cache warnings because the dependencies aren't handled by the correct order, to fix it you run `./build.sh RecordHints` several types until the warnings go away before the build and then the build will go smoothly.

# Directories

**Note:** directories marked with *(G)* are generated and do not appear in the repository.

- `bin` *(G)* - Contains the `Zulib.dll` file used by `zebra` for linking contracts with the Zulib library.
- `fstar` - ZF* library source files.
- `fsharp` - F# source files.
- `fsharp/Realized` - Realized files declared in ZF* and implemented in F#.
- `fsharp/Extracted` *(G)* - Extracted files implemented in ZF*.
- `tests` - Runtime tests.
- `tests/consistency` - Consistency tests.
- `packages` *(G)* - Paket packages.
- `paket-files` *(G)* - Paket files.

# ZF* Modules

## Core

- `prims.fst` - ZF* primitves.
- `FStar.Pervasives.fst` - Basic data types.
- `FStar.Pervasives.Native.fst` - Option and tuples.
- `Zen.Base.fst` - Basic functions.

## Blockchain Entities

- `Zen.TxSkeleton.fsti` - Functions for adding inputs and outputs to transactions.

    (**Note:** this is probably the most important module in Zulib, since it handles the main thing contracts do, and as such - it is typically used by most contracts)

- `Zen.Types.fst` - Code-gen module for all `Zen.Types` modules.
- `Zen.Types.Realized.fsti` - Assumptions for wallet and TxSkeleton.
- `Zen.Types.Extracted.fst` - Basic types used by the contract main function.
- `Zen.Types.Main.fst` - The contract main function and cost function.
- `Zen.Types.Data.fst` - Recursive data type used to represent the contract message body and state.
- `Zen.Data.fst` - Functions and combinators used for extracting specific data types out of the message body and state.
- `Zen.ContractReturn.fst` - Setters for the output of a contract.
- `Zen.ContractResult.fst` - Functions for generating a contract output, typically used in the contract main function to finalize the result.
(**Note:** the `ContractResult` type is a `ContractReturn` wrapped in a `Result`, the actual output type of the main function is  `ContractResult` and because a contract may fail, while the `ContractReturn` is what the contract outputs (wrapped in a `Result`) when the contract execution succeeds.
- `Zen.ContractId.fsti` - Contract ID parsing from a string.
- `Zen.Cost.fst` - Code-gen module for all `Zen.Cost` modules.
- `Zen.Cost.Extracted.fst` - Functions and combinators for working with the `Cost` type (extracted module).
- `Zen.Cost.Realized.fst` - Functions and combinators for working with the `Cost` type (realized module).
- `Zen.Wallet.fsti` - Assumption for a function that gives you the size of a contract wallet.
- `Zen.Asset.fsti` - Generate assets.

## Numberic Types

- `FStar.Int64.fst` - 64 bit signed integers.
- `FStar.UInt64.fst` - 64 bit unsigned integers.
- `FStar.UInt32.fst` - 32 bit unsigned integers.
- `FStar.UInt8.fst` - 8 bit unsigned integers.
- `Zen.Integers.fst` - Unbounded integers.

## Data Structures

**Note:** types who has names comprised of a known type name followd by a `T` (for example - `OptionT` and `ResultT`) are transformed monads (following the convention in Haskell), which are monads handling multiple (usually 2) computational effects at once; in the case of Zulib it typically means the carry the usual effect of the the type along with the `Cost` monad.

See also: [https://en.wikipedia.org/wiki/Monad_transformer](https://en.wikipedia.org/wiki/Monad_transformer)

- `Zen.Option.fst` - Option type ([https://en.wikipedia.org/wiki/Option_type](https://en.wikipedia.org/wiki/Option_type))
- `Zen.OptionT.fst` - Costed Option type (`Cost.t<Option<a>,m>`) - used for handling costed options directly instead of dealing with 2 monads at once.
- `Zen.Result.fst` - Result (sum) type ([https://en.wikipedia.org/wiki/Result_type](https://en.wikipedia.org/wiki/Result_type))
- `Zen.ResultT.fst` - Costed Result type (`Cost.t<Result<a>,m>`) - used for handling costed results directly instead of dealing with 2 monads at once.
- `Zen.Array.Base.fsti` - Code-gen module for all `Zen.Array` modules.
- `Zen.Array.fst` - Basic arrays.
- `Zen.Array.Indexed.fst` - Arrays with a declared length.
- `Zen.List.fst` - Lists (extracted module).
- `Zen.ListRealized.fsti` - Lists (realized module).
- `Zen.Set.fsti` - Sets (based on F# implementation of sets).
- `Zen.Dictionary.fsti` - Dictionaries with string based fields.

## Strings

- `FStar.String.fsti` - Strings.
- `FStar.Char.fsti` - Characters.

## Cryptography

- `Zen.Crypto.fsti` - Basic cryptography functions (public key verification, hashes and public key parsing from strings).
- `Zen.Hash.fst` - (**DEPRECATED**) Code-gen module for all `Zen.Hash` modules.
- `Zen.Hash.Base.fst` - (**DEPRECATED**) Hash parsing.
- `Zen.Hash.Sha3.fsti` - (**DEPRECATED**) Sha3 hashing functions.
- `Zen.Sha3.fst` - Code-gen module for all `Zen.Sha3` modules.

    (**Note:** this module replaces the deprecated `Zen.Hash` modules, to allow the separation of extracted and realized functions)

- `Zen.Sha3.Extracted.fst` - Sha3 hashing functions (extracted module).
- `Zen.Sha3.Realized.fsti` - Sha3 hashing functions (realized module).
- `Zen.PublicKey.fst` - Compressed public keys.
- `Zen.MerkleTree.fsti` - Merkle tree verification.

## Misc

- `Zen.Bitcoin.fsti` - First steps for bitcoin integration.
- `Zen.Util.fsti` - There doesn't seem to be anything in here.