#!/bin/ksh
#

# EXPORT list here
set -x
ulimit -s unlimited
ulimit -a

#export KMP_STACKSIZE=2048M #Overrides common.sh
#export KMP_AFFINITY=scatter

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_FORECAST_FV3

