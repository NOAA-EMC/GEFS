#!/bin/ksh

# EXPORT list here
set -x
export IOBUF_PARAMS=


ulimit -s unlimited
ulimit -a

export MP_SHARED_MEMORY=no
export MEMORY_AFFINITY=core:6

export MP_TASK_AFFINITY=cpu:6
export MP_USE_BULK_XFER=yes
export MP_STDOUTMODE=unordered
export MPICH_ALLTOALL_THROTTLE=0
export MP_COREFILE_FORMAT=core.txt
export OMP_STACKSIZE=3G
export MP_COMPILER=intel

#export NODES=10
#export total_tasks=22
#export OMP_NUM_THREADS=6
#export taskspernode=4

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_INIT_SEPARATE

