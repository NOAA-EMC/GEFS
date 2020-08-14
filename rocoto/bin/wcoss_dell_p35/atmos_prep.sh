#!/bin/bash

set -x
ulimit -s unlimited
ulimit -a

# module_ver.h
. $SOURCEDIR/versions/gefs_wcoss_dell_p35.ver

# Load modules
. /usrx/local/prod/lmod/lmod/init/ksh
module list
module purge

module load EnvVars/$EnvVars_ver
module load ips/$ips_ver
module load impi/$impi_ver
module load prod_util/$prod_util_ver
module load prod_envir/$prod_envir_ver
#module load NetCDF/$NetCDF_ver
module use -a /usrx/local/nceplibs/dev/NCEPLIBS/modulefiles
module load hdf5_parallel/1.10.6
module load netcdf_parallel/4.7.4

module load lsf/$lsf_ver

module load CFP/$CFP_ver
export USE_CFP=YES

module list

# For Development
. $GEFS_ROCOTO/bin/wcoss_dell_p35/common.sh

# Export List
(( OMP_NUM_THREADS_CH = 40 / GEFS_PPN ))
export OMP_NUM_THREADS_CH

$SOURCEDIR/jobs/JGEFS_ATMOS_PREP
