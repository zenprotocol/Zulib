#I @".paket/load/net47"
#I @"packages/"

#load @"Zen.FSharp.Compiler.Service.fsx"
#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler

module Array = Microsoft.FSharp.Collections.Array

let extractedDir = "fsharp/Extracted"
let binDir = "bin"

let (++) = Array.append

let getFiles pattern =
  FileSystemHelper.directoryInfo  FileSystemHelper.currentDirectory
  |> FileSystemHelper.filesInDirMatching pattern
  |> Array.map (fun file -> file.FullName)

let zulibFiles = getFiles "fstar/*.fst" ++ getFiles "fstar/*.fsti"

let getHints() = getFiles "fstar/*.hints" ++ getFiles "fstar/*.checked"

let zipHints(): unit =
    Fake.ZipHelper.Zip "fstar" "fstar/Hints.zip" <| getHints()

let unzipHints(): unit =
    Fake.ZipHelper.Unzip "fstar" "fstar/Hints.zip"

let clearHints(): unit =
    Fake.FileHelper.DeleteFiles <| getHints()

let runFStar args files =

  let join = Array.reduce (fun a b -> a + " " + b)

  let primsFile = "\"" + FileSystemHelper.currentDirectory + "/fstar/prims.fst" + "\""
  let files = Array.map (fun f -> "\"" + f + "\"") files

  let executable,fstarPath,z3Path =
    if EnvironmentHelper.isLinux then ("mono", "packages/ZFStar/tools/fstar.exe", "packages/zen_z3_linux/output/z3-linux")
    elif EnvironmentHelper.isMacOS then ("mono", "packages/ZFStar/tools/fstar.exe", "packages/zen_z3_osx/output/z3-osx")
    else ("packages/ZFStar/tools/fstar.exe","","packages/zen_z3_windows/output/z3.exe")

  let fstar = [|
    fstarPath;
    "--smt";z3Path;
    "--prims";primsFile;
    "--no_default_includes";
    "--include";"fstar/"; |]
  //printfn "%s" (join (fstar ++ args ++ zulibFiles));
  ProcessHelper.Shell.AsyncExec (executable, join (fstar ++ args ++ files))

Target "Clean" (fun _ ->
  CleanDir extractedDir
  CleanDir binDir
)

Target "RecordHints" (fun _ ->
  let args =
    [| //"--z3refresh";
       //"--verify_all";
       "--record_hints"
       "--cache_checked_modules" |]
  //clearHints();
  //unzipHints();
  let exitCodes = zulibFiles |> Array.map (fun file -> runFStar args [|file|])
                             |> Async.Parallel
                             |> Async.RunSynchronously
  if exitCodes |> Array.forall ((=) 0)
  then zipHints()
  else failwith "recording Zulib hints failed"

)

Target "Verify" (fun _ ->
  let args =
    [| "--use_hints";
       //"--strict_hints";
       "--use_hint_hashes"
       "--cache_checked_modules"
    |]
  clearHints();
  unzipHints();
  let exitCodes = zulibFiles |> Array.map (fun file -> runFStar args [|file|])
                             |> Async.Parallel
                             |> Async.RunSynchronously
  if not (Array.forall (fun exitCode -> exitCode = 0) exitCodes)
    then failwith "Verifying Zulib failed"
)


Target "Extract" (fun _ ->
  let cores = System.Environment.ProcessorCount
  let threads = cores * 2

  let args =
    [|
       "--lax";
       //"--cache_checked_modules"
       //"--use_hints";
       //"--use_hint_hashes";
       "--codegen";"FSharp";
       "--extract_module";"Zen.Integers";
       "--extract_module";"Zen.Base";
       "--extract_module";"Zen.Result";
       "--extract_module";"Zen.ResultT";
       "--extract_module";"Zen.Option";
       "--extract_module";"Zen.OptionT";
       "--extract_module";"Zen.Optics";
       "--extract_module";"Zen.Array.Indexed";
       "--extract_module";"Zen.Cost.Extracted";
       "--codegen-lib";"Zen.Cost";
       "--extract_module";"Zen.List";
       "--extract_module";"Zen.ListBounded";
       "--codegen-lib";"Zen.Array";
       "--extract_module";"Zen.Types.Extracted";
       "--extract_module";"Zen.Types.Data";
       "--extract_module";"Zen.Types.Main";
       "--codegen-lib";"Zen.Types";
       "--extract_module";"Zen.Hash.Base";
       "--codegen-lib";"Zen.Hash"
       "--extract_module";"Zen.Data";
       "--extract_module";"Zen.ContractReturn";
       "--extract_module";"Zen.ContractResult";
       //"--extract_module"; "Zen.Wallet"
       "--odir";extractedDir |]

  let exitCode = runFStar args zulibFiles
                 |> Async.RunSynchronously

  if exitCode <> 0 then
    failwith "extracting Zulib failed"
)

Target "Build" (fun _ ->

  let files =
    [| "fsharp/Realized/prims.fs";
      "fsharp/Realized/FStar.Pervasives.fs";
      "fsharp/Extracted/Zen.Base.fs";
      "fsharp/Extracted/Zen.Option.fs";
      "fsharp/Extracted/Zen.Result.fs";
      "fsharp/Realized/Zen.Cost.Realized.fs";
      "fsharp/Extracted/Zen.Cost.Extracted.fs";
      "fsharp/Extracted/Zen.Integers.fs";
      "fsharp/Realized/FStar.UInt8.fs";
      "fsharp/Realized/FStar.UInt32.fs";
      "fsharp/Realized/FStar.UInt64.fs";
      "fsharp/Realized/FStar.Int64.fs";
      "fsharp/Realized/FStar.Char.fs";
      "fsharp/Realized/Zen.Set.fs";
      "fsharp/Extracted/Zen.OptionT.fs";
      "fsharp/Extracted/Zen.ResultT.fs";
      "fsharp/Extracted/Zen.List.fs";
      "fsharp/Extracted/Zen.ListBounded.fs";
      "fsharp/Realized/Zen.Array.Base.fs";
      "fsharp/Realized/FStar.String.fs";
      "fsharp/Realized/Zen.Dictionary.fs";
      "fsharp/Extracted/Zen.Array.Indexed.fs";
      "fsharp/Extracted/Zen.Types.Extracted.fs";
      "fsharp/Extracted/Zen.Types.Data.fs";
      "fsharp/Realized/Zen.Types.Realized.fs";
      "fsharp/Realized/Zen.Util.fs";
      "fsharp/Realized/Zen.ContractId.fs";
      "fsharp/Realized/Zen.Asset.fs";
      "fsharp/Extracted/Zen.Types.Main.fs";
      "fsharp/Realized/Zen.Wallet.fs";
      //"fsharp/Extracted/Zen.Wallet.fs";
      "fsharp/Realized/Zen.TxSkeleton.fs";
      "fsharp/Extracted/Zen.Data.fs";
      "fsharp/Extracted/Zen.ContractReturn.fs";
      "fsharp/Extracted/Zen.ContractResult.fs";
      "fsharp/Realized/Zen.Hash.Sha3.fs";
      "fsharp/Realized/Zen.Crypto.fs";
      "fsharp/Extracted/Zen.Hash.Base.fs";
      "fsharp/Realized/Zen.MerkleTree.fs";
      "fsharp/Realized/Zen.Bitcoin.fs";
    |]

  let checker = FSharpChecker.Create()

  let frameworkDirectory =
    if EnvironmentHelper.isLinux then ("/usr/lib/mono/4.7-api/")
    elif EnvironmentHelper.isMacOS then ("/Library/Frameworks/Mono.framework/Versions/Current/lib/mono/4.7-api/")
    else ("C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.7\\")

  let fw = sprintf "%s%s" frameworkDirectory

  let compileParams =
    [|
      "fsc.exe" ; "-o"; "bin/Zulib.dll"; "-a";
      "--noframework";
      "-r"; fw "mscorlib.dll";
      "-r"; fw "System.Core.dll";
      "-r"; fw "System.dll";
      "-r"; fw "System.Numerics.dll";
      "-r"; "packages/FSharp.Core/lib/net45/FSharp.Core.dll"
      "-r"; "packages/FSharp.Compatibility.OCaml/lib/net45/FSharp.Compatibility.OCaml.dll"
      //"-r"; "../../packages/libsodium-net/lib/Net40/Sodium.dll"
      "-r"; "packages/FSharpx.Collections/lib/net40/FSharpx.Collections.dll"
      "-r"; "packages/BouncyCastle/lib/BouncyCastle.Crypto.dll"
      "-r"; "packages/FsBech32/lib/net45/FsBech32.dll"
    |]

  let messages, exitCode =
    Async.RunSynchronously (checker.Compile (Array.append compileParams files))

  if exitCode <> 0 then
    let errors = Array.filter (fun (msg:FSharpErrorInfo) -> msg.Severity = FSharpErrorSeverity.Error) messages
    printfn "%A" errors
    failwith "building Zulib failed"
    )

Target "Test" (fun _ ->
    let fsharpi (fsx: string) =
        if EnvironmentHelper.isWindows
        then ProcessHelper.Shell.AsyncExec("fsi.exe", fsx)
        else ProcessHelper.Shell.AsyncExec("fsharpi", fsx)

    let tests = getFiles "tests/*.fsx"

    let exitCodes = tests |> Array.map fsharpi
                          |> Async.Parallel
                          |> Async.RunSynchronously
    if not (Array.forall ((=) 0) exitCodes)
    then failwith "One or more tests failed"
)

Target "Default" ( fun _ ->
    Run "Clean"
    Run "Verify"
    Run "Extract"
    Run "Build"
    Run "Test"
    )

(*
"Clean"
  ==> "Verify"
  ==> "Extract"
  ==> "Build"
  ==> "Default"
*)

RunTargetOrDefault "Default"
