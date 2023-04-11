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
module list
module purge

module use -a /scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack

module load hpc/$hpc_ver

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

ver=${ver:-$(echo ${gefs_ver}|cut -c1-5)}
export COMOUT=${COMOUT:-${COMROOT}/gefs/$ver/${RUN}.${PDY}/$cyc}
export ROTDIR=${COMROOT}/gefs/${ver}

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_ATMOS_PREP
