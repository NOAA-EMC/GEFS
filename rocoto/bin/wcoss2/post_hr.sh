#!/bin/ksh

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
module load grib_util/$grib_util_ver
module load NetCDF/$NetCDF_ver
module load HDF5-serial/$HDF5_serial_ver

module load lsf/$lsf_ver

#    $module load util_shared/1.1.0
module load g2tmpl/$g2tmpl_ver

module load CFP/$CFP_ver
export USE_CFP=YES

module list

# For Development
. $GEFS_ROCOTO/bin/wcoss2/common.sh

# Export List
#export POSTGRB2TBL=$G2TMPL_SRC/params_grib2_tbl_new

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_ATMOS_POST
