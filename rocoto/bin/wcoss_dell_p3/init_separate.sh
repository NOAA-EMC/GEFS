#!/bin/ksh

# EXPORT list here
set -x
ulimit -s unlimited
ulimit -a

export KMP_STACKSIZE=2048M # Overrides common.sh
export KMP_AFFINITY=scatter

#export OMP_NUM_THREADS=${GEFS_TPP:-6}

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_INIT_SEPARATE
