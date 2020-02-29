#!/bin/ksh
#

# EXPORT list here
set -x
export IOBUF_PARAMS=


ulimit -s unlimited
ulimit -a

export MP_SHARED_MEMORY=no
export MEMORY_AFFINITY=core:6

#export total_tasks=6
#export OMP_NUM_THREADS=6
#export taskspernode=4

export FORECAST_SEGMENT=hr

export NTHREADS_SIGCHGRS=6

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_INIT_RECENTER

