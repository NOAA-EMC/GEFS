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

#module load EnvVars/$EnvVars_ver
#module load ips/$ips_ver
#module load impi/$impi_ver
#module load prod_util/$prod_util_ver
#module load prod_envir/$prod_envir_ver

#module load lsf/$lsf_ver
module load python/$python_ver
module load HPSS/$HPSS_ver

module list

# For Development
. $GEFS_ROCOTO/bin/wcoss_dell_p35/common.sh

# Export List
#export NTHREADS_SIGCHGRS=${GEFS_TPP:-6}
export COMIN=${COMIN:-${COMROOT}/${NET}/${envir}/${RUN}.${PDY}/$cyc}
export COMOUT=${COMOUT:-${COMROOT}/${NET}/${envir}/${RUN}.${PDY}/$cyc}

# CALL executable job script here
$GEFS_ROCOTO/bin/py/archive_chem.py
