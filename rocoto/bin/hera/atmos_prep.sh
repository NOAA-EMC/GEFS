#! /usr/bin/env bash

set -x
ulimit -s unlimited
ulimit -a

# module_ver.h
. $GEFS_ROCOTO/dev/versions/run_hera.ver

# Load modules
module reset

module use -a /scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack

module load hpc/${hpc_ver}
module load hpc-intel/${intel_ver}
module load hpc-impi/${impi_ver}
module load grib_util/${grib_util_ver}
module load prod_util/${prod_util_ver}
module load netcdf/${netcdf_ver}

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

shopt -s extglob
ver=${gefs_ver%${gefs_ver##v+([0-9]).+([0-9])}}
shopt -u extglob

export ROTDIR=${COMROOT}/gefs/${ver}
export ROTDIR_GFS=${HOMEdata}/gfs/${gfs_ver}

# CALL executable job script here
${SOURCEDIR}/jobs/JGEFS_ATMOS_PREP
