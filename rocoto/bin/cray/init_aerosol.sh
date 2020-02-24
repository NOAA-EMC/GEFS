#!/bin/ksh
#

set -x

export IOBUF_PARAMS=cfi*:size=64M:count=4:verbose
export FORT_BUFFERED=TRUE
export MKL_CBWR=AVX
ulimit -s unlimited
ulimit -a

export APRUN_CHGRES="aprun -j 1 -n 1 -N 1 -d 1 -cc depth"
export APRUN_CALCINC="aprun -j -1 -n 1 -N 1 -d 1 -cc depth"

$SOURCEDIR/jobs/JGEFS_INIT_AEROSOL