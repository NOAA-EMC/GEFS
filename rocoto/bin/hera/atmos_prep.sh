#!/bin/bash
#

# EXPORT list here
set -x
export IOBUF_PARAMS=


ulimit -s unlimited
ulimit -a

export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=core:2

#export NODES=1
#export total_tasks=12
#export OMP_NUM_THREADS=2
#export taskspernode=12

export FORECAST_SEGMENT=hr

# Export List
(( OMP_NUM_THREADS_CH = 40 / GEFS_PPN ))
export OMP_NUM_THREADS_CH

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_ATMOS_PREP
