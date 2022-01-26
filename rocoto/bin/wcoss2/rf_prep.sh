#!/bin/ksh -l

set -x
ulimit -s unlimited
ulimit -a

#export OMP_NUM_THREADS=${GEFS_TPP:-6}

export OMP_NUM_THREADS=1
export envir=prod
# CALL executable job script here
$SOURCEDIR/rocoto/bin/sh/JGEFS_RF_PREP

