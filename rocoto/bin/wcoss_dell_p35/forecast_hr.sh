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

module load EnvVars/$EnvVars_ver
module load ips/$ips_ver
module load impi/$impi_ver
module load prod_util/$prod_util_ver
module load prod_envir/$prod_envir_ver
module load NetCDF/$NetCDF_ver
module load HDF5-serial/$HDF5_serial_ver

module load lsf/$lsf_ver

module load CFP/$CFP_ver
export USE_CFP=YES

module list

# For Development
. $GEFS_ROCOTO/bin/wcoss_dell_p35/common.sh

# Export List
#export KMP_STACKSIZE=2048M #Overrides common.sh
#export KMP_AFFINITY=scatter
#export RERUN="YES"

export COMIN=${COMIN:-${COMROOT}/${NET}/${envir}/${RUN}.${PDY}/$cyc}
export COMOUT=${COMOUT:-${COMROOT}/${NET}/${envir}/${RUN}.${PDY}/$cyc}
export GESIN=${GESIN:-${COMROOT}/${NET}/${envir}/${RUN}.${PDY}/$cyc/nwges}
export GESOUT=${GESOUT:-${COMROOT}/${NET}/${envir}/${RUN}.${PDY}/$cyc/nwges}

if [[ $cplwav = ".true." ]]; then
    # Set location of wave restart from last cycle
    export WRDIR=${COMROOT}/${NET}/${envir}
fi


# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_FORECAST

