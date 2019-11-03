#!/bin/ksh
#

# EXPORT list here
set -x

ulimit -s unlimited
ulimit -a

export MPICH_ABORT_ON_ERROR=1
export MPICH_ENV_DISPLAY=1
export MPICH_VERSION_DISPLAY=1
export MPICH_CPUMASK_DISPLAY=1

export KMP_STACKSIZE=1024m
export OMP_NUM_THREADS=2
export KMP_AFFINITY=disabled

#export OMP_NUM_THREADS=4
export KMP_AFFINITY=disabled

export MP_EUIDEVICE=sn_all
export MP_EUILIB=us
export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=core:2

. /opt/modules/default/init/ksh
module load NetCDF-intel-haswell/4.2
module load nco-gnu-sandybridge/4.4.4


export NODES=2
export total_tasks=31
export OMP_NUM_THREADS=2
export taskspernode=12

export gefsmpexec_mpmd="  aprun -b -j${NODES} -n$total_tasks -N$taskspernode  -d$OMP_NUM_THREADS -cc depth  cfp mpmd_cmdfile"


# export for development runs only begin
export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}

. $GEFS_ROCOTO/parm/setbase
. $GEFS_ROCOTO/parm/gefs_config
. $GEFS_ROCOTO/parm/gefs_dev.parm


# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_EXTRACTVARS

