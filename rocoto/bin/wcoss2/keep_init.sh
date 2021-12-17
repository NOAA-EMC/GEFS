#!/bin/ksh -l

set -x
ulimit -s unlimited
ulimit -a

# module_ver.h
. $SOURCEDIR/versions/run.ver

# Load modules
. /usrx/local/prod/lmod/lmod/init/ksh
module list
module purge

#module load EnvVars/$EnvVars_ver
#module load ips/$ips_ver
#module load impi/$impi_ver
#module load prod_util/$prod_util_ver
#module load prod_envir/$prod_envir_ver

#module load lsf/$lsf_ver
module load python/$python_ver

module list

# For Development
. $GEFS_ROCOTO/bin/wcoss2/common.sh

# Export List
#export NTHREADS_SIGCHGRS=${GEFS_TPP:-6}
export OMP_NUM_THREADS=1
export envir=prod

# CALL executable job script here
$GEFS_ROCOTO/bin/py/keep_init.py
