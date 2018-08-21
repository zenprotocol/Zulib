#!/bin/sh

chmod +x packages/zen_z3_linux/output/z3-linux
chmod +x packages/zen_z3_osx/output/z3-osx

cp ./packages/metadatafix/System.Reflection.Metadata/lib/portable-net45+win8/System.Reflection.Metadata.dll ./packages/FAKE/tools/
cp Zen.FSharp.Compiler.Service.dll.config ./packages/FAKE/tools/
cp Zen.FSharp.Compiler.Service.dll.config ./packages/Zen.FSharp.Compiler.Service/lib/net45/
cp FAKE.exe.config ./packages/FAKE/tools/

exit_code=$?
  if [ $exit_code -ne 0 ]; then
    exit $exit_code
  fi

ulimit -n 2000

# Arguments passed to F# interactive
FsiArgs=""
# Arguments passed to build.fsx
ScriptArgs="$@"
# Fake run options
FakeRunOpts=''
# Fake options
FakeOpts='--removeLegacyFakeWarning'

# Uncomment below to enable mono debug
#FsiArgs="$FsiArgs -d:MONO"

if [ "$FsiArgs" ]
then FsiArgs="--fsiargs $FsiArgs"
fi

FakeRunOpts="$FakeRunOpts $FsiArgs"

if [ "$ScriptArgs" ]
then ScriptArgs="-- $ScriptArgs"
fi

FakeCommand="mono ./packages/FAKE/tools/FAKE.exe $FakeOpts run $FakeRunOpts build.fsx $ScriptArgs"
echo "FAKE command:\n    $FakeCommand\n"
# Execute the Fake command
$FakeCommand
