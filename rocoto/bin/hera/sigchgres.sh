#!/bin/ksh

# EXPORT list here
set -x
export NODES=1
export IOBUF_PARAMS=


ulimit -s unlimited
ulimit -a

export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=core:4

export total_tasks=1
export OMP_NUM_THREADS=4
export taskspernode=1

export FORECAST_SEGMENT=hr

# export for development runs only begin
export NTHREADS_SIGCHGRS=6

cd $SOURCEDIR/control

#. $SOURCEDIR/parm/gefs.parm

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_SIGCHGRS

