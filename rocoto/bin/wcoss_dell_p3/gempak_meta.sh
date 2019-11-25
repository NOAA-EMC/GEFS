#!/bin/ksh
#

# EXPORT list here
set -x

export IOBUF_PARAMS=cfi*:size=64M:count=4:verbose
export FORT_BUFFERED=TRUE
export MKL_CBWR=AVX
ulimit -s unlimited
ulimit -a

#export gefsmpexec_mpmd="mpirun -n $total_tasks cfp mpmd_cmdfile"

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_GEMPAK_META

