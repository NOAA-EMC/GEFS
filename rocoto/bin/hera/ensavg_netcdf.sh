#! /usr/bin/env bash

set -x
ulimit -s unlimited
ulimit -a

# module_ver.h
. ${GEFS_ROCOTO}/dev/versions/run_hera.ver

# Load modules
module reset

module use -a /scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack

module load hpc/${hpc_ver}
module load hpc-intel/${intel_ver}
module load hpc-impi/${impi_ver}
module load grib_util/${grib_util_ver}
module load prod_util/${prod_util_ver}
module load netcdf/${netcdf_ver}

module load prod_envir/${prod_envir_ver}

module list

# For Development
. ${GEFS_ROCOTO}/bin/hera/common.sh

# Export List
#export MP_SHARED_MEMORY=no
#export MEMORY_AFFINITY=core:6

#export total_tasks=6
#export OMP_NUM_THREADS=6
#export taskspernode=4

export FORECAST_SEGMENT=hr

export NTHREADS_SIGCHGRS=6

# CALL executable job script here
${SOURCEDIR}/jobs/JGEFS_ATMOS_ENSAVG_NETCDF

