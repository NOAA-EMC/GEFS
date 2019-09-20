#!/bin/ksh

# EXPORT list here
set -x
export IOBUF_PARAMS=*:size=32M:count=4:verbose


ulimit -s unlimited
ulimit -a




export OMP_NUM_THREADS=1

#export OMP_NUM_THREADS=1

export MP_SHARED_MEMORY=no
export MEMORY_AFFINITY=core:1

export NODES=3
export total_tasks=21
export OMP_NUM_THREADS=1
export taskspernode=7

# export for development runs only begin

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_POST_TRACK
