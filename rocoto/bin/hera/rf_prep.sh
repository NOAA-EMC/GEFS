#!/bin/ksh
#

# EXPORT list here
set -x

ulimit -s unlimited
ulimit -a

#export OMP_NUM_THREADS=6

# export for development runs only begin

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_RF_PREP

