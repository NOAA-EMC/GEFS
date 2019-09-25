#!/bin/ksh

# EXPORT list here
set -x
ulimit -s unlimited
ulimit -a

export KMP_AFFINITY=disabled

# export total_tasks=32
export OMP_NUM_THREADS=6
# export taskspernode=4
# export NODES=7

# export for development runs only begin
# export envir=${envir:-dev}
# export RUN_ENVIR=${RUN_ENVIR:-dev}

# export gefsmpexec=" mpirun -n $total_tasks "
export NTHREADS_SIGCHGRS=6

# export APRUN=${gefsmpexec:-mpirun}
# export APRUN_CHGRES=${APRUN_CHGRES:-"mpirun -n 1 "}
# export APRUN_RECENT=${APRUN_RECENT:-"mpirun -n $total_tasks "}

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_INIT_PROCESS
