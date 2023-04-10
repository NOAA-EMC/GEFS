#!/bin/bash
#

# EXPORT list here
set -x
#export IOBUF_PARAMS=
ulimit -s unlimited
ulimit -a

# module_ver.h
. $GEFS_ROCOTO/dev/versions/run_hera.ver

# Load modules
. /apps/lmod/lmod/init/ksh
module list
module purge

module load EnvVars/$EnvVars_ver
module load intel/$ips_ver
module load impi/$impi_ver
module load prod_util/$prod_util_ver
module load prod_envir/$prod_envir_ver
module load hdf5_parallel/$HDF5_parallel_ver
module load netcdf_parallel/$NetCDF_parallel_ver

module list

# For Development
. $GEFS_ROCOTO/bin/hera/common.sh


export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=core:2

#export NODES=1
#export total_tasks=12
#export OMP_NUM_THREADS=2
#export taskspernode=12

#export FORECAST_SEGMENT=hr

# Export List
(( OMP_NUM_THREADS_CH = 40 / GEFS_PPN ))
export OMP_NUM_THREADS_CH

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_ATMOS_PREP
