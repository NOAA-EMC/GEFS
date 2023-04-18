#! /usr/bin/env bash

set -x
ulimit -s unlimited
ulimit -a

# module_ver.h
. $SOURCEDIR/versions/run.ver

# Load modules
module purge
module load envvar/$envvar_ver
module load PrgEnv-intel/$PrgEnv_intel_ver
module load craype/$craype_ver
module load intel/$intel_ver

module load cray-mpich/$cray_mpich_ver
module load cray-pals/$cray_pals_ver

module load prod_util/$prod_util_ver
module load prod_envir/$prod_envir_ver

#module load libjpeg/$libjpeg_ver
#module load grib_util/$grib_util_ver

module load netcdf/$netcdf_ver
module load hdf5/$hdf5_ver
module load cfp/$cfp_ver
export USE_CFP=YES

module list

# For Development
. $GEFS_ROCOTO/bin/wcoss2/common.sh

# Export List
export OMP_NUM_THREADS=1

#export RERUN="YES"

export envir=prod
# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_FORECAST

