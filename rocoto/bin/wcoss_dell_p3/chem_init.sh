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

module load NCO/$NCO_ver
module load python/$python_ver

module load lsf/$lsf_ver

module load HDF5-parallel/$HDF5_parallel_ver
module load NetCDF-parallel/$NetCDF_parallel_ver

module list

# For Development
. $GEFS_ROCOTO/bin/wcoss_dell_p3/common.sh

# Export List

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_CHEM_INIT
