#!/bin/ksh

set -x
ulimit -s unlimited
ulimit -a

# module_ver.h
. $GEFS_ROCOTO/dev/versions/gefs_wcoss_dell_p35.ver

# Load modules
. /usrx/local/prod/lmod/lmod/init/ksh
module list
module purge

module load ips/$ips_ver
module load grib_util/$grib_util_ver
module load prod_util/$prod_util_ver
module load prod_envir/$prod_envir_ver
module load NetCDF/$NetCDF_ver

module list

# For Development
. $GEFS_ROCOTO/bin/wcoss_dell_p35/common.sh

export COMOUT=${COMOUT:-${COMROOT}/${NET}/${envir}/${RUN}.${PDY}/$cyc/atmos}

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_ATMOS_GETCFSSST

