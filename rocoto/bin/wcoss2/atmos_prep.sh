#!/bin/bash

set -x
ulimit -s unlimited
ulimit -a

# module_ver.h
. $SOURCEDIR/versions/gefs_wcoss2.ver

# Load modules
. /usrx/local/prod/lmod/lmod/init/ksh
module list
module purge

module load EnvVars/$EnvVars_ver
module load ips/$ips_ver
module load impi/$impi_ver
module load prod_util/$prod_util_ver
module load prod_envir/$prod_envir_ver
module load HDF5-parallel/$HDF5_parallel_ver
module load NetCDF-parallel/$NetCDF_parallel_ver

module load lsf/$lsf_ver

module load CFP/$CFP_ver
export USE_CFP=YES

module list

# For Development
. $GEFS_ROCOTO/bin/wcoss2/common.sh

# Export List
(( OMP_NUM_THREADS_CH = 40 / GEFS_PPN ))
export OMP_NUM_THREADS_CH

$SOURCEDIR/jobs/JGEFS_ATMOS_PREP
