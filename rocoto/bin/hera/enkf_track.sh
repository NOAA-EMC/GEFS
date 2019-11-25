#!/bin/ksh

# EXPORT list here
set -x
export IOBUF_PARAMS=


ulimit -s unlimited
ulimit -a

#export OMP_NUM_THREADS=1

export MP_SHARED_MEMORY=no
export MEMORY_AFFINITY=core:1

#export total_tasks=21
#export OMP_NUM_THREADS=6
#export taskspernode=4

# export for development runs only begin

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_ENKF_TRACK
