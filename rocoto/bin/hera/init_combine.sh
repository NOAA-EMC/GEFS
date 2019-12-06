#!/bin/ksh

# EXPORT list here
set -x
export IOBUF_PARAMS=


ulimit -s unlimited
ulimit -a

export OMP_NUM_THREADS=1

export MP_SHARED_MEMORY=no
export MEMORY_AFFINITY=core:1

#export NODES=20
#export total_tasks=40
#export OMP_NUM_THREADS=1
#export taskspernode=2

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_INIT_COMBINE

