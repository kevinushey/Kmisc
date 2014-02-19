#!/usr/bin/env sh

IN_KMISC=`pwd | grep Kmisc$`
if [ ${#IN_KMISC} == 0 ]; then
    echo "Error: not in base Kmisc directory."
    exit 0
fi;

## NOTE: For some reason, clang-modernize tries to add a second
## set of overrides to Rstreambuf.h, so we exclude it explicitly

## I also currently get assertion errors if I try to use 
## '-use-auto', hence I explicitly set some parameters for modernizing

clang-check src/*.cpp \
    -analyze \
    -- \
    -std=c++1y \
    -I/Users/kevinushey/.llvm/libcxx/include \
    -I/Library/Frameworks/R.framework/Headers \
    -I/Library/Frameworks/R.framework/Resources/library/Rcpp/include \
    -Iinst/include
    