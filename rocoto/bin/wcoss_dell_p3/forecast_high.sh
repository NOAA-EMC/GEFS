#!/bin/ksh
#

# EXPORT list here
set -x
ulimit -s unlimited
ulimit -a

# export MPI_LABELIO=YES
# export MP_STDOUTMODE=ORDERED
export KMP_STACKSIZE=2048M #Overrides common.sh
export KMP_AFFINITY=scatter

#export FORECAST_SEGMENT=hr

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_FORECAST_FV3

