#!/bin/ksh
#

# EXPORT list here
set -x
export IOBUF_PARAMS=


ulimit -s unlimited
ulimit -a

export OMP_NUM_THREADS=2

export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=core:2

export NODES=1
export total_tasks=12
export OMP_NUM_THREADS=2
export taskspernode=12

export FORECAST_SEGMENT=hr

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_INIT_FV3CHGRS
