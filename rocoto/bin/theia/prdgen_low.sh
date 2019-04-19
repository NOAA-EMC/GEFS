#!/bin/ksh
#

# EXPORT list here
set -x
export IOBUF_PARAMS=*:size=64M:count=4:verbose


ulimit -s unlimited
ulimit -a




export OMP_NUM_THREADS=4

#export OMP_NUM_THREADS=4

export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=core:4

export NODES=1
export total_tasks=6
export OMP_NUM_THREADS=4
export taskspernode=6

export DO_LOW_RES=


# export for development runs only begin

# CALL executable job script here
. $SOURCEDIR/jobs/JGEFS_PRDGEN
