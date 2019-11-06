#!/bin/ksh

# EXPORT list here
set -x
export IOBUF_PARAMS=
ulimit -s unlimited
ulimit -a

export MP_SHARED_MEMORY=no
export MEMORY_AFFINITY=core:6

export total_tasks=32
export OMP_NUM_THREADS=6
export taskspernode=4

# export for development runs only begin
export NTHREADS_SIGCHGRS=6

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_INIT_PROCESS

