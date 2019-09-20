#!/bin/ksh

# EXPORT list here
set -x
export IOBUF_PARAMS=


ulimit -s unlimited
ulimit -a




export OMP_NUM_THREADS=1

#export OMP_NUM_THREADS=1

export MP_SHARED_MEMORY=no
export MEMORY_AFFINITY=core:1

export NODES=1
export total_tasks=16
export OMP_NUM_THREADS=1
export taskspernode=16

export FORECAST_SEGMENT=lr

# export for development runs only begin

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_CQPF
