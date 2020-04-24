#!/bin/ksh

set -x
ulimit -s unlimited
ulimit -a

# module_ver.h
. $SOURCEDIR/versions/gefs_wcoss_dell_p3.ver

# Load modules
. /usrx/local/prod/lmod/lmod/init/ksh
module list
module purge

module load EnvVars/$EnvVars_ver
module load ips/$ips_ver
module load impi/$impi_ver
module load prod_util/$prod_util_ver
module load prod_envir/$prod_envir_ver
module load NetCDF/$NetCDF_ver

module load lsf/$lsf_ver

module list

# For Development
. $GEFS_ROCOTO/bin/wcoss_dell_p3/common.sh

# Export List
export OMP_NUM_THREADS_CH=${GEFS_TPP:-28}


$SOURCEDIR/jobs/JGEFS_ATMOS_INIT_FV3CHGRS
