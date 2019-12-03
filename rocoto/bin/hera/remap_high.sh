#!/bin/ksh
#

# EXPORT list here
set -x
export NODES=4
export IOBUF_PARAMS=


ulimit -s unlimited
ulimit -a

#export OMP_NUM_THREADS=2

export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=core:2

. /opt/modules/default/init/ksh
module load NetCDF-intel-haswell/4.2
module load nco-gnu-sandybridge/4.4.4

#export total_tasks=48
#export OMP_NUM_THREADS=2
#export taskspernode=12

export FORECAST_SEGMENT=hr

# export for development runs only begin
export gefsmachine=theia
export NTHREADS_SIGCHGRS=6

cd $SOURCEDIR/control

#. $SOURCEDIR/parm/gefs.parm

# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_FV3REMAP

