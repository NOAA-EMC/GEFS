#!/bin/ksh -l

set -x
ulimit -s unlimited
ulimit -a

# module_ver.h
. $SOURCEDIR/versions/run.ver

# Load modules
module purge

module load envvar/$envvar_ver
module load intel/$intel_ver
module load prod_util/$prod_util_ver
module load prod_envir/$prod_envir_ver

module list

# For Development
. $GEFS_ROCOTO/bin/wcoss2/common.sh

export OMP_NUM_THREADS=1
export envir=prod

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_ATMOS_POST_CLEANUP
