#I @".paket/load/net47"
#load @"Fake.Core.Process.fsx"
#load @"Fake.Core.Target.fsx"
#load @"Fake.Core.CommandLineParsing.fsx"
#load @"Fake.IO.FileSystem.fsx"
#load @"Fake.IO.Zip.fsx"
#load @"FSharpPlus.fsx"
#load @"FSharpx.Extras.fsx"
#load @"Zen.FSharp.Compiler.Service.fsx"

//#if !FAKE
//let execContext = Fake.Core.Context.FakeExecutionContext.Create false "build.fsx" []
//Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)
//#endif

module Async = FSharpx.Async
module Array = Microsoft.FSharp.Collections.Array
module Context = Fake.Core.Context
module CoreTracing = Fake.Core.CoreTracing
module DirectoryInfo = Fake.IO.DirectoryInfo
module Environment = Fake.Core.Environment
module File = Fake.IO.File
module FileInfo = Fake.IO.FileInfo
module FileSystem = Fake.IO.FileSystem
module Path = Fake.IO.Path
module Process = Fake.Core.Process
module Seq = FSharpx.Collections.Seq
module Shell = Fake.IO.Shell
module Target = Fake.Core.Target
module Zip = Fake.IO.Zip

type Async = Microsoft.FSharp.Control.Async
type ExecParams = Fake.Core.ExecParams
type FakeExecutionContext = Context.FakeExecutionContext
type FSharpChecker = Microsoft.FSharp.Compiler.SourceCodeServices.FSharpChecker
type FSharpErrorSeverity = Microsoft.FSharp.Compiler.SourceCodeServices.FSharpErrorSeverity
type RuntimeContext = Context.RuntimeContext
type Target = Fake.Core.Target
type OS =
    | Linux
    | MacOS
    | Windows

#if !FAKE
printfn "NOFAKE"
#endif

// If we are running outside of a FAKE context, we need to create one
#if !FAKE
fsi.CommandLineArgs
|> Seq.skipWhile ((<>) "--") // Fake prefixes fsi args with "--", so we ignore before this
|> List.ofSeq
|> FakeExecutionContext.Create false "build.fsx"
|> RuntimeContext.Fake
|> Context.setExecutionContext
#endif

let context: FakeExecutionContext =
    Context.getExecutionContext()
    |> Context.getFakeExecutionContext
    |> function | Some context -> context
                | None -> failwith "failed to initialize FAKE context"
printfn "%A" (context.Arguments)
(*
let OS = if   Environment.isLinux   then Linux
         elif Environment.isMacOS   then MacOS
         elif Environment.isWindows then Windows
         else failwith "Only Linux, MacOS, and Windows are supported."

let (^) = FSharpx.Functional.Prelude.op_Concatenate
let (++) = FSharpPlus.Operators.op_PlusPlus
let (>>=) = FSharpPlus.Operators.op_GreaterGreaterEquals
let (</>) = Fake.IO.FileSystemOperators.op_LessDivideGreater
let flip = FSharpx.Functional.Prelude.flip

// perform an asynchronous action `f x`, and return `x`
let asyncTee (f: 'A -> Async<unit>) (x: 'A): Async<'A> =
    async { do! f x;
            return x }

// inserts an element at the last position in a sequence
let insert (xs: seq<'A>) (x: 'A): seq<'A> = xs ++ seq [x]

// concatenates a sequence of strings, interspersing them with a space
let interspace: seq<string> -> string = String.concat " "

// concatenates a sequence of strings, interspersing them with a path seperator
let concatPath: seq<string> -> string = Seq.fold (</>) ""

// logs a string asynchronously
let asyncLog (s: string): Async<unit> =
    async { printfn "%s" s }

let rootDir = Shell.pwd()
let binDir = rootDir </> "bin"
let fsharpDir = rootDir </> "fsharp"
let fstarDir = rootDir </> "fstar"
let packagesDir = rootDir </> "packages"
let testsDir = rootDir </> "tests"
let extractedDir = fsharpDir </> "Extracted"
let realizedDir = fsharpDir </> "Realized"
let primsPath = fstarDir </> "prims.fst"
let fstarExePath = concatPath [ packagesDir; "ZFStar"; "tools"; "fstar.exe" ]
let z3Path =
    match OS with
    | Linux -> concatPath [ packagesDir; "zen_z3_linux"; "output"; "z3-linux" ]
    | MacOS -> concatPath [ packagesDir; "zen_z3_osx"; "output"; "z3-osx" ]
    | Windows -> concatPath [ packagesDir; "zen_z3_windows"; "output"; "z3.exe" ]
let frameworkDir =
    match OS with
    | Linux -> concatPath [ "/usr"; "lib"; "mono"; "4.7-api" ]
    | MacOS -> concatPath [ "/Library"; "Frameworks"; "Mono.framework"; "Versions"; "Current"; "lib"; "mono"; "4.7-api" ]
    | Windows -> concatPath [ "C:"; "Program Files (x86)"; "Reference Assemblies"; "Microsoft"; "Framework"; ".NETFramework"; "v4.7"; "" ]

// Gets fully qualified filepaths in `dir`
let getFiles (dir: string):  seq<string> =
    DirectoryInfo.ofPath dir
    |> DirectoryInfo.getFiles
    |> Seq.map FileInfo.(|FullName|)

// Checks if a filepath has any of the specified extensions, including leading .
let extensionIsOneOf (exts: seq<string>) (filepath: string): bool =
    Seq.exists (flip Path.hasExtension filepath) exts

// Gets files in `dir` with any extension in `exts`
let getFilesWithExtension (exts: seq<string>) (dir: string): seq<string> =
    getFiles dir |> Seq.filter ^ extensionIsOneOf exts

// F* source files
let fstarFiles: seq<string> = getFilesWithExtension [".fst"; ".fsti"] fstarDir
// Test files
let testFiles: seq<string> = getFilesWithExtension [".fsx"] testsDir
// Gets hints and cached checked modules
let getHints(): seq<string> = getFilesWithExtension [".hints"; ".checked"] fstarDir
// Zips hints and cached checked modules into fstar/Hints.zip
let zipHints(): unit = getHints() |> Zip.zip "fstar" "fstar/Hints.zip"
// Unzips hints and cached checked modules from fstar/Hints.zip to fstar/
let unzipHints(): unit = Zip.unzip "fstar" "fstar/Hints.zip"
// Deletes all hints and cached checked modules from fstar/
let cleanHints(): unit = getHints() |> File.deleteAll

let default_fstar_args: seq<string> =
    seq [ "--smt"; z3Path
          "--prims"; primsPath
          "--no_default_includes"
          "--include"; fstarDir ]

// creates exec params, given a program and a sequence of arguments
let execParams (program: string) (args: seq<string>): ExecParams =
  { Program=program; CommandLine=interspace args; Args=[]; WorkingDir=rootDir }

// logs async params, asynchronously
let logExecParams (execParams: ExecParams): Async<unit> =
    sprintf "%s %s" execParams.Program execParams.CommandLine
    |> asyncLog

// ExecParams for fstar.exe with defaults on args
let execFstarParams (args: seq<string>): ExecParams =
    let fstar_args = Seq.concat [default_fstar_args; args]
    match OS with
    | Linux | MacOS -> execParams "mono" ^ Seq.cons fstarExePath fstar_args
    | Windows -> execParams fstarExePath fstar_args

// Runs fstar.exe with defaults on args, returning the exit code.
let fstar_exe' (args: seq<string>): Async<int> =
    (execFstarParams args)
    |> asyncTee logExecParams
    >>= Process.asyncShellExec

// Runs fstar.exe with defaults and opts on files,
// returning a bool indicating success.
let fstar_exe (opts: seq<string>) (files: seq<string>): bool =
    let fstar_args = Seq.append opts files
    Async.RunSynchronously (fstar_exe' fstar_args) = 0

// Spawns a new fstar.exe with defaults on args for each file,
// returning a bool indicating success.
let parallel_fstar_exe (args: seq<string>) (files: seq<string>): bool =
    Seq.map (insert args) files
    |> Seq.map fstar_exe'
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.forall ^ (=) 0

let fsharpi (files: seq<string>): bool =
    let command = match OS with
                  | Linux | MacOS -> "fsharpi"
                  | Windows -> "fsc.exe"
    Seq.map (fun file -> Fake.Core.Shell.AsyncExec(command, file)) files
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.forall ^ (=) 0

let clean() = List.iter Shell.cleanDir [binDir; extractedDir]

let regenHintsOpts = seq [ "--record_hints";
                            "--cache_checked_modules" ]

let regenHints() = match fstar_exe regenHintsOpts fstarFiles with
                    | true -> zipHints()
                    | false -> failwith "Recording hints failed"

let verifyOpts = seq [ "--use_hints";
                       "--use_hint_hashes";
                       "--use_cached_modules" ]

let verify() = match parallel_fstar_exe verifyOpts fstarFiles with
               | true -> zipHints()
               | false -> failwith "Verifying zulib failed"

let realizedModules = seq [ "Zen.Array.Base";
                            "Zen.Asset";
                            "Zen.Bitcoin";
                            "Zen.ContractId";
                            "Zen.Cost.Realized";
                            "Zen.Crypto";
                            "Zen.Dictionary";
                            "Zen.Hash.Sha3";
                            "Zen.MerkleTree";
                            "Zen.Set";
                            "Zen.TxSkeleton";
                            "Zen.Types.Realized";
                            "Zen.Util"
                            "Zen.Wallet" ]

let noExtract' (moduleName: string): seq<string> =
    seq [ "--extract"; sprintf "'-%s'" moduleName ]

let noExtract (moduleNames: seq<string>): seq<string> =
    Seq.collect noExtract' moduleNames

let extractOpts = seq [ "--lax";
                        "--use_hints";
                        "--use_hint_hashes";
                        "--use_cached_modules";
                        "--codegen"; "FSharp";
                        "--odir"; extractedDir;
                        "--extract"; "'Zen'" ]
                  ++ noExtract realizedModules

let extract() = match fstar_exe extractOpts fstarFiles with
                | true -> ()
                | false -> failwith "Extraction failed:"

let compileFiles =
    let r (fileName: string): string = realizedDir  </> fileName
    let e (fileName: string): string = extractedDir </> fileName
    seq [ r "prims.fs";
          r "FStar.Pervasives.fs";
          e "Zen.Base.fs";
          e "Zen.Option.fs";
          e "Zen.Result.fs";
          r "Zen.Cost.Realized.fs";
          e "Zen.Cost.Extracted.fs";
          e "Zen.Integers.fs";
          r "FStar.UInt8.fs";
          r "FStar.UInt32.fs";
          r "FStar.UInt64.fs";
          r "FStar.Int64.fs";
          r "FStar.Char.fs";
          r "Zen.Set.fs";
          e "Zen.OptionT.fs";
          e "Zen.ResultT.fs";
          e "Zen.List.fs";
          r "Zen.Array.Base.fs";
          r "FStar.String.fs";
          r "Zen.Dictionary.fs";
          e "Zen.Array.Indexed.fs";
          e "Zen.Types.Extracted.fs";
          e "Zen.Types.Data.fs";
          r "Zen.Types.Realized.fs";
          r "Zen.Util.fs";
          r "Zen.ContractId.fs";
          r "Zen.Asset.fs";
          e "Zen.Types.Main.fs";
          r "Zen.Wallet.fs";
          r "Zen.TxSkeleton.fs";
          e "Zen.Data.fs";
          e "Zen.ContractReturn.fs";
          e "Zen.ContractResult.fs";
          r "Zen.Hash.Sha3.fs";
          r "Zen.Crypto.fs";
          e "Zen.Hash.Base.fs";
          r "Zen.MerkleTree.fs";
          r "Zen.Bitcoin.fs" ]

let compileArgs =
    seq [ "fsc.exe"; "-o"; binDir </> "Zulib.dll"; "-a"; "--noframework" ]

let compileRefs =
    seq [ frameworkDir </> "mscorlib.dll";
          frameworkDir </> "System.Core.dll";
          frameworkDir </> "System.dll";
          frameworkDir </> "System.Numerics.dll";
          concatPath [ packagesDir; "FSharp.Core"; "lib"; "net45"; "FSharp.Core.dll" ]
          concatPath [ packagesDir; "FSharp.Compatibility.OCaml"; "lib"; "net45"; "FSharp.Compatibility.OCaml.dll" ]
          concatPath [ packagesDir; "FSharpx.Collections"; "lib"; "net40"; "FSharpx.Collections.dll" ]
          concatPath [ packagesDir; "BouncyCastle"; "lib"; "BouncyCastle.Crypto.dll" ]
          concatPath [ packagesDir; "FsBech32"; "lib"; "net45"; "FsBech32.dll" ]
        ]

let reference (dllPath: string): seq<string> = seq ["-r"; dllPath]
let references (dllPaths: seq<string>): seq<string> =
    Seq.collect reference dllPaths

let compile() =
    let checker = FSharpChecker.Create()
    let checkerArgs = Seq.concat [ compileArgs; references compileRefs; compileFiles]
                      |> Array.ofSeq
    match Async.RunSynchronously (checker.Compile checkerArgs) with
    | _, 0 -> ()
    | messages, _ ->
        messages
        |> Array.filter (fun msg -> msg.Severity = FSharpErrorSeverity.Error)
        |> printfn "%A"
        failwith "building Zulib failed"

let test() =
    if not ^ fsharpi testFiles then failwith "One or more tests failed"

let target (name: string) (body: unit -> unit) =
    Target.create name (fun _ -> body())

target "Clean" clean
target "CleanHints" cleanHints
target "RegenHints" regenHints
target "Verify" verify
target "Extract" extract
target "Compile" compile
target "Build" (clean >> verify >> extract >> compile)

let commandLineArgs =
    fsi.CommandLineArgs
*)
printfn "%A" (fsi.CommandLineArgs)

//Target.listAvailable()
//test()
