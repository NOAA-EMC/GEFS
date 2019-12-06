#!/bin/ksh

# EXPORT list here
set -x
ulimit -s unlimited
ulimit -a

export KMP_AFFINITY=disabled

#export OMP_NUM_THREADS=${GEFS_TPP:-6}

export NTHREADS_SIGCHGRS=${GEFS_TPP:-6}

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_INIT_PROCESS
