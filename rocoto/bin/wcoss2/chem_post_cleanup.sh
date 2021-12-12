#!/bin/ksh

set -x
ulimit -s unlimited
ulimit -a

# module_ver.h
. $SOURCEDIR/versions/run.ver

# Load modules
. /usrx/local/prod/lmod/lmod/init/ksh
module list
module purge

module load EnvVars/$EnvVars_ver
module load ips/$ips_ver
module load prod_util/$prod_util_ver
module load prod_envir/$prod_envir_ver

module list

# For Development
. $GEFS_ROCOTO/bin/wcoss2/common.sh

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_CHEM_POST_CLEANUP
