#!/bin/ksh
#

# EXPORT list here
set -x
ulimit -s unlimited
ulimit -a

#export KMP_AFFINITY=disabled


# export aprun_gec00="mpirun -n 1 "
#export NTHREADS_SIGCHGRS=${GEFS_TPP:-6}

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_INIT_RECENTER
