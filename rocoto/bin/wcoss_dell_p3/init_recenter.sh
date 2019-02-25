#!/bin/ksh
#

# EXPORT list here
set -x
export NODES=7

ulimit -s unlimited
ulimit -a

export KMP_AFFINITY=disabled

export total_tasks=20
export OMP_NUM_THREADS=7
export taskspernode=4

# export for development runs only begin
export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}

export gefsmpexec_mpmd="  mpirun -n 756 cfp mpmd_cmdfile"
export APRUNC="mpirun"
export APRUN_RECENT="mpirun -n $total_tasks"
export APRUN_CHGRES="mpirun -n 1"

export aprun_gec00="mpirun -n 1 "
export NTHREADS_SIGCHGRS=6

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_INIT_RECENTER
