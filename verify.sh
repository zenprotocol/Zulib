#!/bin/sh

#mono packages/ZFStar/tools/fstar.exe \
mono ../FStar/bin/fstar.exe \
     --smt packages/zen_z3_linux/output/z3-linux \
     --prims fstar/prims.fst \
     --no_default_includes \
     --include $PWD/fstar \
     --use_cached_modules \
     --use_hints \
     $@
