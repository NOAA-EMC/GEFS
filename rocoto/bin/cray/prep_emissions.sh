#!/bin/ksh
#

set -x

export IOBUF_PARAMS=“*:verbose”
# export IOBUF_PARAMS=
# export FORT_BUFFERED=TRUE
export MKL_CBWR=AVX
ulimit -s unlimited
ulimit -a

export gefsmpexec="aprun -b -j1 -n1 -N1 -d24 -cc depth"

$SOURCEDIR/jobs/JGEFS_PREP_EMISSIONS