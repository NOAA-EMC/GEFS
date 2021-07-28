#!/bin/ksh

set -x
ulimit -s unlimited
ulimit -a

#export OMP_NUM_THREADS=${GEFS_TPP:-6}

# CALL executable job script here
$SOURCEDIR/rocoto/bin/sh/JGEFS_RF_PREP

