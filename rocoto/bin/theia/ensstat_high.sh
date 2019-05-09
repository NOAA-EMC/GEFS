#!/bin/ksh

# EXPORT list here
set -x
export IOBUF_PARAMS=cfi*:size=64M:count=4:verbose


ulimit -s unlimited
ulimit -a





export OMP_NUM_THREADS=4

#export OMP_NUM_THREADS=4

export MP_SHARED_MEMORY=no
export MEMORY_AFFINITY=core:4

export total_tasks=3
export OMP_NUM_THREADS=4
export taskspernode=3

export FORECAST_SEGMENT=hr

# export for development runs only begin

# CALL executable job script here
. $SOURCEDIR/jobs/JGEFS_ENSSTAT

