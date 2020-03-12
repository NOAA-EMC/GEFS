#!/bin/ksh
#

# EXPORT list here
set -x
ulimit -s unlimited
ulimit -a

#export OMP_NUM_THREADS_CH=${GEFS_TPP:-28}

#export FORECAST_SEGMENT=hr

$SOURCEDIR/jobs/JGEFS_INIT_FV3CHGRS
