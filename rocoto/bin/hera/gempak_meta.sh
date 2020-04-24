#!/bin/ksh
#

# EXPORT list here
set -x

export IOBUF_PARAMS=cfi*:size=64M:count=4:verbose
export FORT_BUFFERED=TRUE
export MKL_CBWR=AVX
ulimit -s unlimited
ulimit -a

#export total_tasks=8
#export OMP_NUM_THREADS=1
#export taskspernode=8

# export for development runs only begin
#export envir=${envir:-dev}
#export RUN_ENVIR=${RUN_ENVIR:-dev}

#export gefsmpexec_mpmd="mpirun -n $total_tasks cfp mpmd_cmdfile"

#. $GEFS_ROCOTO/parm/setbase
#. $GEFS_ROCOTO/parm/gefs_config
#. $GEFS_ROCOTO/parm/gefs_dev.parm

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_ATMOS_GEMPAK_META

