#!/bin/ksh
#

set -x

# export IOBUF_PARAMS=cfi*:size=64M:count=4:verbose
# export FORT_BUFFERED=TRUE
# export MKL_CBWR=AVX
ulimit -s unlimited
ulimit -a

$SOURCEDIR/jobs/JGEFS_INIT_AEROSOL