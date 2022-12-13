#!/bin/ksh -l

set -x
ulimit -s unlimited
ulimit -a

# module_ver.h
. $SOURCEDIR/versions/run.ver

# Load modules
module purge
module load envvar/$envvar_ver
module load intel/$intel_ver
module load prod_util/$prod_util_ver
module load prod_envir/$prod_envir_ver
module load libjpeg/$libjpeg_ver
module load grib_util/$grib_util_ver
module load wgrib2/$wgrib2_ver

module list

# For Development
. $GEFS_ROCOTO/bin/wcoss2/common.sh

if [[ 1 == 0 ]]; then # when tests atmos_awips tasks only, it needs add pgrb2ap5[sp25] files
    export COMPONENT=${COMPONENT:-atmos}
    ver=${ver:-$(echo ${gefs_ver}|cut -c1-5)}
    export COMOUT=${COMOUT:-$(compath.py -o $NET/${ver})/${RUN}.${PDY}/$cyc}
    if [ ! -d $COMOUT/atmos ]; then
        mkdir -p $COMOUT/atmos
    fi
    if [ ! -d $COMOUT/atmos/pgrb2sp25 ]; then
        ln -s /lfs/h1/ops/prod/com/gefs/v12.3/gefs.${PDY}/$cyc/atmos/pgrb2sp25 $COMOUT/atmos/pgrb2sp25
    fi
    if [ ! -d $COMOUT/atmos/pgrb2ap5 ]; then
        ln -s /lfs/h1/ops/prod/com/gefs/v12.3/gefs.${PDY}/$cyc/atmos/pgrb2ap5 $COMOUT/atmos/pgrb2ap5
    fi
fi

# Export List
export OMP_NUM_THREADS=1
export envir=prod

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_ATMOS_AWIPS
